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
	/*===========================*/

	interface Ifc_registerfile;
	  method ActionValue#(Operands) opaddress(Bit#(5) rs1addr, Op1type rs1type, Bit#(5) rs2addr, 
        Op2type rs2type, Bit#(VADDR) pc, Bit#(32) imm `ifdef spfpu ,Bit#(5) rs3addr, Op3type
        rs3type `endif );

		`ifdef Debug
      method ActionValue#(Bit#(XLEN)) read_write_gprs(Bit#(5) r, Bit#(XLEN) data, Bool rw 
          `ifdef spfpu ,Op3type rfselect `endif );
		`endif
		method Action write_rd(Bit#(5) r, Bit#(XLEN) d `ifdef spfpu , Op3type rdtype `endif );
    `ifdef RV64 
      method Action inferred_xlen (Bool xlen); 
    `endif // False-32bit,  True-64bit 
	endinterface

	(*synthesize*)
	module mkregisterfile(Ifc_registerfile);
    Integer verbosity = `VERBOSITY;
		RegFile#(Bit#(5),Bit#(XLEN)) integer_rf <-mkRegFileWCF(0,31);
		`ifdef spfpu 
			RegFile#(Bit#(5),Bit#(XLEN)) floating_rf <-mkRegFileWCF(0,31);
		`endif
		Reg#(Bool) initialize<-mkReg(True);
		Reg#(Bit#(5)) rg_index<-mkReg(0);
    `ifdef RV64
      Wire#(Bool) wr_xlen <-mkWire();
    `endif

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

	  method ActionValue#(Operands) opaddress(Bit#(5) rs1addr, Op1type rs1type, Bit#(5) rs2addr, 
        Op2type rs2type, Bit#(VADDR) pc, Bit#(32) imm `ifdef spfpu ,Bit#(5) rs3addr, Op3type
        rs3type `endif ) if(!initialize);
			
			Bit#(XLEN) rs1irf=integer_rf.sub(rs1addr);
			Bit#(XLEN) rs2irf=integer_rf.sub(rs2addr);
      `ifdef spfpu
  			Bit#(XLEN) rs1frf=floating_rf.sub(rs1addr);
	  		Bit#(XLEN) rs2frf=floating_rf.sub(rs2addr);
		  	Bit#(XLEN) rs3frf=floating_rf.sub(rs3addr);
      `endif

      Bit#(XLEN) rs1, rs2 `ifdef spfpu , rs3 `endif ;

      if(rs1type==PC)
        rs1=zeroExtend(pc);
      `ifdef spfpu
        else if(rs1type==FloatingRF)
          rs1=rs1frf;
      `endif
      else
        rs1=rs1irf;

      if(rs2type==Constant4)
        rs2='d4;
      else if(rs2type==Immediate)
        rs2=signExtend(imm);
      `ifdef spfpu
        else if(rs2type==FloatingRF)
          rs2=rs2frf;
      `endif
      else
        rs2=rs2irf;

      `ifdef spfpu
        if(rs3type==FloatingRF)
          rs3=rs3frf;
        else
          rs3=0;
      `endif
        
      `ifdef RV64
        if(!wr_xlen) begin // if XLEN>MXLEN but MXLEN is set to 32
          rs1=signExtend(rs1[31:0]);
          rs2=signExtend(rs2[31:0]);
        end
      `endif

			if(verbosity>0)
        $display($time,"\nReg1 :%d(%h) ",rs1addr,rs1,fshow(rs1type),"\nReg2 : %d(%h) ",rs2addr,
          rs2,fshow(rs2type) `ifdef spfpu ,"\nReg3: %d(%h) ; ",rs3addr,rs3,fshow(rs3type) `endif ); 

      return Operands{rs1:rs1,rs2:rs2 `ifdef spfpu ,rs3: rs3 `endif };
		endmethod

    `ifdef RV64 
      method Action inferred_xlen (Bool xlen); 
        wr_xlen<= xlen;
      endmethod  
    `endif // False-32bit,  True-64bit 

		method Action write_rd(Bit#(5) r, Bit#(XLEN) d `ifdef spfpu , Op3type rdtype `endif ) 
                                                                                    if(!initialize);
			if(verbosity>0)
        $display($time,"\tRF: Writing Rd: %d(%h) ",r,d `ifdef spfpu ,fshow(rdtype) `endif ); 

      `ifdef spfpu
        if(rdtype==FloatingRF)begin
				  floating_rf.upd(r,d);
        end
        else
      `endif
				if(r!=0)begin
					integer_rf.upd(r,d);
				end
		endmethod
		`ifdef Debug
      method ActionValue#(Bit#(XLEN)) read_write_gprs(Bit#(5) r, Bit#(XLEN) data, Bool rw 
          `ifdef spfpu ,Op3type rfselect `endif ) if(!initialize);
          Bit#(XLEN) resultop=0;
          if(rw) begin // write_operation
            `ifdef spfpu
              if(rfselect==FloatingRF)
                floating_rf.upd(r, data);
              else
            `endif
                integer_rf.upd(r, data);
          end
          else begin // read operation
            `ifdef spfpu
              if(rfselect==FloatingRF)
                resultop=floating_rf.sub(r);
              else
            `endif
                resultop=integer_rf.sub(r);
          end
        return resultop;
      endmethod
		`endif
	endmodule
endpackage
