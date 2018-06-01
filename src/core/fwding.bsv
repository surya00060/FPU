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

--------------------------------------------------------------------------------------------------
*/
package fwding;
  import common_types::*;
  `include "common_params.bsv"
  import Vector::*;

  interface Ifc_fwding;
    method ActionValue#(FwdType#(XLEN)) read_rs1 (Bit#(5) addr, Bit#(XLEN) rfvalue   
                                                            `ifdef spfpu , Op3type rstype `endif );
    method ActionValue#(FwdType#(XLEN)) read_rs2 (Bit#(5) addr, Bit#(XLEN) rfvalue   
                                                            `ifdef spfpu , Op3type rstype `endif );
    `ifdef spfpu                                                            
    method ActionValue#(FwdType#(XLEN)) read_rs3 (Bit#(5) addr, Bit#(XLEN) rfvalue, Op3type rstype );
    `endif

		method ActionValue#(Bit#(TLog#(PRFDEPTH))) get_index(Bit#(5) addr
      `ifdef spfpu , Op3type rdtype `endif );
		method Action fwd_from_exe (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
		method Action fwd_from_mem (Tuple2#(Bit#(XLEN),  Bit#(TLog#(PRFDEPTH))) in);
    method Action flush_mapping;
		method Action update_rd (Bit#(TLog#(PRFDEPTH)) index);
  endinterface

  (*synthesize*)
  (*conflict_free="fwd_from_exe, fwd_from_mem"*)
  (*conflict_free="fwd_from_exe, update_rd"*)
  (*conflict_free="update_rd, fwd_from_mem"*)
  module mkfwding(Ifc_fwding);
    Reg#(FwdType#(XLEN)) data [valueOf(PRFDEPTH)];
    `ifdef spfpu
		  Vector#(PRFDEPTH,Reg#(Tuple2#(Bit#(5),Op3type))) mapping<-replicateM(mkReg(tuple2(0,InRF)));
    `else
		  Vector#(PRFDEPTH,Reg#(Bit#(5))) mapping<-replicateM(mkReg(0));
    `endif
		Reg#(Bit#(TLog#(PRFDEPTH))) rg_index<-mkReg(0);
    for(Integer i=0;i<valueOf(PRFDEPTH);i=i+1)begin
      data[i] <- mkReg(tagged Absent);
    end


    method ActionValue#(FwdType#(XLEN)) read_rs1 (Bit#(5) addr, Bit#(XLEN) rfvalue   
                                                            `ifdef spfpu , Op3type rstype `endif );
      FwdType#(XLEN) ret= tagged Present rfvalue;
			let array_rd=readVReg(mapping); // convert the reg-vector to bit#(5)-vector
      `ifdef spfpu
			  let index=findElem(tuple2(addr,rstype),array_rd); // find the index of match
        if(rstype==IRF && addr==0)
          return tagged Present 0;
        else begin
				  if(index matches tagged Valid .idx)begin // if match exists
            ret = data[idx];
          end
          return ret;
        end
      `else
			  let index=findElem(addr,array_rd); // find the index of match
        if(addr==0)
          return tagged Present 0;
        else begin
				  if(index matches tagged Valid .idx)begin // if match exists
            ret = data[idx];
          end
          return ret;
        end
      `endif
                    
    endmethod
    method ActionValue#(FwdType#(XLEN)) read_rs2 (Bit#(5) addr, Bit#(XLEN) rfvalue   
                                                            `ifdef spfpu , Op3type rstype `endif );
      FwdType#(XLEN) ret= tagged Present rfvalue;
			let array_rd=readVReg(mapping); // convert the reg-vector to bit#(5)-vector
      `ifdef spfpu
			  let index=findElem(tuple2(addr,rstype),array_rd); // find the index of match
        if(rstype==IRF && addr==0)
          return tagged Present 0;
        else begin
				  if(index matches tagged Valid .idx)begin // if match exists
            ret = data[idx];
          end
          return ret;
        end
      `else
			  let index=findElem(addr,array_rd); // find the index of match
        if(addr==0)
          return tagged Present 0;
        else begin
				  if(index matches tagged Valid .idx)begin // if match exists
            ret = data[idx];
          end
          return ret;
        end
      `endif
                    
    endmethod
    `ifdef spfpu
    method ActionValue#(FwdType#(XLEN)) read_rs3 (Bit#(5) addr, Bit#(XLEN) rfvalue   
                                                            Op3type rstype );
      FwdType#(XLEN) ret= tagged Present rfvalue;
			let array_rd=readVReg(mapping); // convert the reg-vector to bit#(5)-vector
      if(rstype==IRF && addr==0)
        return tagged Present 0;
      else begin
			  if(index matches tagged Valid .idx)begin // if match exists
          ret = data[idx];
        return ret;
      end
    endmethod
    `endif
		method ActionValue#(Bit#(TLog#(PRFDEPTH))) get_index(Bit#(5) addr
          `ifdef spfpu , Op3type rdtype `endif );
      rg_index<= rg_index+1;
			let array_rd=readVReg(mapping);
      `ifdef spfpu
			  let index=findElem(tuple2(addr,rdtype),array_rd); // find the index of match
        mapping[rg_index]<= tuple2(addr, rdtype);
			  if(index matches tagged Valid .idx)begin
				  if(pack(idx)!=rg_index)
					  mapping[idx]<=tuple2(0,IRF);
      `else
			  let index=findElem(addr,array_rd); // find the index of match
        mapping[rg_index]<= addr;
			  if(index matches tagged Valid .idx)begin
				  if(pack(idx)!=rg_index)
					  mapping[idx]<=0;
        end
      `endif
      return rg_index;
    endmethod
		method Action fwd_from_exe (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
			data[index]<=tagged Present d;	
		endmethod
		method Action fwd_from_mem (Tuple2#(Bit#(XLEN),  Bit#(TLog#(PRFDEPTH))) in);
      let {index, d}=in;
			data[index]<=tagged Present d;	
		endmethod
		method Action update_rd (Bit#(TLog#(PRFDEPTH)) index);
			data[index]<=tagged Absent;
		endmethod
    method Action flush_mapping;
      for(Integer i=0;i<valueOf(PRFDEPTH);i=i+1)begin
        `ifdef spfpu
          mapping[i]<= tuple2(0, IRF);
        `else 
          mapping[i]<= 0;
        `endif
      end
    endmethod
  endmodule
endpackage
