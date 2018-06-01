/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and/or other materials provided 
 with the distribution.  
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Author: Neel Gala
Email id: neelgala@gmail.com
Details:
This module implements the integer and floating point register files. They are currently implemented
as RegFile. The integer register file requires 2 read and 1 write ports.
The floating point registerfile however will require 3 read ports and 1 write ports

On system reset,  the register files are initialized to 0. This phase will take 32 cycles total.
Only after the initialization phase can the 

the debug interface allows the debugger to read/write from/to either of the registerfiles. 
This interface should be made mutually exclusive with respect to the other rules accessing the
register files,  otherwise they will require dedicated extra ports. This scheduling is done
implicitly by bluespec owing to the sequence in which the methods have been written,  The debugger
however cannot read the values in the initialization phase.

--------------------------------------------------------------------------------------------------
*/
package registerfile;
	/*==== Project Imports === */
	import common_types::*;
	`include "common_params.bsv"
	/*======================== */
	/*===== Package Imports ==== */
	import RegFile::*;
	import ConfigReg::*;
  import GetPut::*;
	/*===========================*/

	interface Ifc_registerfile;
	  method ActionValue#(Operands) opaddress( Bit#(5) rs1addr, Bit#(5) rs2addr, Bit#(5) rd
        `ifdef spfpu , Op1type rs1type , Op2type rs2type , Bit#(5) rs3addr , Op3type rs3type 
        `endif );

		`ifdef Debug
      method ActionValue#(Bit#(XLEN)) read_write_gprs(Bit#(5) r, Bit#(XLEN) data, Bool rw 
          `ifdef spfpu ,Op3type rfselect `endif );
		`endif
    interface Put#(CommitData) commit_rd;
    method Action get_index(Bit#(2) index);
    method Action reset_renaming;
	endinterface

	(*synthesize*)
	module mkregisterfile(Ifc_registerfile);
    Integer verbosity = `VERBOSITY;
		RegFile#(Bit#(5),Bit#(XLEN)) integer_rf <-mkRegFileWCF(0,31);
    Reg#(Maybe#(Bit#(2))) arr_rename_int [32];
		`ifdef spfpu 
			RegFile#(Bit#(5),Bit#(XLEN)) floating_rf <-mkRegFileWCF(0,31);
      Reg#(Maybe#(Bit#(2))) arr_rename_float [32];
		`endif
		Reg#(Bool) initialize<-mkReg(True);
		Reg#(Bit#(5)) rg_index<-mkReg(0);
    Wire#(Bit#(2)) wr_rename_index <- mkDWire(3);

    for (Integer i=0;i<32;i=i+1) begin
      arr_rename_int[i]<- mkReg(tagged Invalid);
      `ifdef spfpu
        arr_rename_float[i]<- mkReg(tagged Invalid);
      `endif
    end


    // The following rule is fired on system reset and writes all the register values to "0". This
    // rule will never fire otherwise
		rule initialize_regfile(initialize);
		  `ifdef spfpu
			  floating_rf.upd(rg_index,0);
		  `endif
			integer_rf.upd(rg_index,0);
			rg_index<=rg_index+1;
			if(rg_index=='d31)
				initialize<=False;
		endrule

	  method ActionValue#(Operands) opaddress( Bit#(5) rs1addr, Bit#(5) rs2addr, Bit#(5) rd
        `ifdef spfpu , Op1type rs1type , Op2type rs2type , Bit#(5) rs3addr , Op3type rs3type ,
        Op3type rdtype `endif ) if(!initialize);
			
			Bit#(XLEN) rs1irf=integer_rf.sub(rs1addr);
			Bit#(XLEN) rs2irf=integer_rf.sub(rs2addr);
      `ifdef spfpu
  			Bit#(XLEN) rs1frf=floating_rf.sub(rs1addr);
	  		Bit#(XLEN) rs2frf=floating_rf.sub(rs2addr);
		  	Bit#(XLEN) rs3frf=floating_rf.sub(rs3addr);
      `endif

      Bit#(XLEN) rs1, rs2 `ifdef spfpu , rs3 `endif ;

      Bit#(2) rs1index=fromMaybe(3, arr_rename_int[rs1addr]); 
      Bit#(2) rs2index=fromMaybe(3, arr_rename_int[rs2addr]); 

      `ifdef spfpu
        Bit#(2) rs3index=fromMaybe(3, arr_rename_int[rs3addr]); 
        if(rs1type==FloatingRF)begin
          rs1=rs1frf;
          rs1index=fromMaybe(3, arr_rename_float[rs1addr]); 
        end
        else 
      `endif
        rs1=rs1irf;

      `ifdef spfpu
        if(rs2type==FloatingRF)begin
          rs2=rs2frf;
          rs2index=fromMaybe(3, arr_rename_float[rs2addr]); 
        end
        else
      `endif
        rs2=rs2irf;

      `ifdef spfpu
        if(rs3type==FRF)
          rs3=rs3frf;
        else
          rs3=signExtend(imm);
      `endif
      `ifdef spfpu
        if(rdtype==FRF)
          arr_rename_float[rd]<= tagged Valid wr_rename_index; 
        else
      `endif
        arr_rename_int[rd]<= tagged Valid wr_rename_index; 
        
      `ifdef spfpu
        return tuple7(rs1, rs2, rs3, rs1index, rs2index, rs3index, wr_rename_index);
      `else
        return tuple5(rs1, rs2, rs1index, rs2index, wr_rename_index);
      `endif
		endmethod

    interface commit_rd= interface Put
		method Action put (CommitData in) if(!initialize);
      `ifdef spfpu
        let{r, d, index, rdtype}=in;
      `else
        let{r, d, index}=in;
      `endif
			if(verbosity>0)
        $display($time,"\tRF: Writing Rd: %d(%h) ",r,d `ifdef spfpu ,fshow(rdtype) `endif ); 

      `ifdef spfpu
        if(rdtype==FRF)begin
				  floating_rf.upd(r,d);
          if(arr_rename_float[r] matches tagged Valid .x &&& x == index)
            arr_rename_float[r]<= tagged Invalid;
        end
        else
      `endif
				if(r!=0)begin
					integer_rf.upd(r,d);
          if(arr_rename_int[r] matches tagged Valid .x &&& x == index)
            arr_rename_int[r]<= tagged Invalid;
				end
		endmethod
    endinterface;
		`ifdef Debug
      method ActionValue#(Bit#(XLEN)) read_write_gprs(Bit#(5) r, Bit#(XLEN) data, Bool rw 
          `ifdef spfpu ,Op3type rfselect `endif ) if(!initialize);
          Bit#(XLEN) resultop=0;
          if(rw) begin // write_operation
            `ifdef spfpu
              if(rfselect==FRF)
                floating_rf.upd(r, data);
              else
            `endif
                integer_rf.upd(r, data);
          end
          else begin // read operation
            `ifdef spfpu
              if(rfselect==FRF)
                resultop=floating_rf.sub(r);
              else
            `endif
                resultop=integer_rf.sub(r);
          end
        return resultop;
      endmethod
		`endif
    method Action get_index(Bit#(2) index);
      wr_rename_index<=index;
    endmethod
    method Action reset_renaming;
      for (Integer i=0;i<32;i=i+1) begin
        arr_rename_int[i]<= tagged Invalid;
        `ifdef spfpu
          arr_rename_float[i]<= tagged Invalid;
        `endif
      end
    endmethod
	endmodule
endpackage
