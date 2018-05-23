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
package fwding1;
  import common_types::*;
  `include "common_params.bsv"
  import Vector::*;
  import GetPut::*;

  interface Ifc_fwding;
    method ActionValue#(FwdType#(XLEN)) read_rs1 (Bit#(XLEN) rfvalue, Bit#(TLog#(PRFDEPTH)) index);
    method ActionValue#(FwdType#(XLEN)) read_rs2 (Bit#(XLEN) rfvalue, Bit#(TLog#(PRFDEPTH)) index);
    `ifdef spfpu                                              
    method ActionValue#(FwdType#(XLEN)) read_rs3 (Bit#(XLEN) rfvalue, Bit#(TLog#(PRFDEPTH)) index);
    `endif

		method Action fwd_from_exe (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
		method Action fwd_from_mem (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
    interface Get#(Bit#(TLog#(PRFDEPTH))) get_index;
    method Action flush_mapping;
  endinterface

  (*synthesize*)
  (*conflict_free="fwd_from_exe, fwd_from_mem"*)
  (*conflict_free="fwd_from_exe, get_index_get"*)
  (*conflict_free="get_index_get, fwd_from_mem"*)
  module mkfwding(Ifc_fwding);
    Reg#(FwdType#(XLEN)) fwd_data [valueOf(PRFDEPTH)-1];
    for(Integer i=0;i<= 32;i=i+ 1)begin
      if(i<valueOf(PRFDEPTH)-1)
        fwd_data[i]<- mkReg(tagged Absent); 
    end
    Reg#(Bit#(TLog#(PRFDEPTH))) rg_index <- mkReg(0);
    method Action flush_mapping;
      for(Integer i=0;i<valueOf(PRFDEPTH)-1;i=i+1)begin
        fwd_data[i]<= tagged Absent;
      end
    endmethod
    method ActionValue#(FwdType#(XLEN)) read_rs1 (Bit#(XLEN) rfvalue, Bit#(TLog#(PRFDEPTH)) index);
      FwdType#(XLEN) ret= tagged Present rfvalue;
      if(index!=3)
        ret=fwd_data[index];
      return ret;
    endmethod
    method ActionValue#(FwdType#(XLEN)) read_rs2 (Bit#(XLEN) rfvalue, Bit#(TLog#(PRFDEPTH)) index);
      FwdType#(XLEN) ret= tagged Present rfvalue;
      if(index!=3)
        ret=fwd_data[index];
      return ret;
    endmethod
    `ifdef spfpu                                                            
    method ActionValue#(FwdType#(XLEN)) read_rs3 (Bit#(XLEN) rfvalue, Bit#(TLog#(PRFDEPTH)) index);
      FwdType#(XLEN) ret= tagged Present rfvalue;
      if(index!=3)
        ret=fwd_data[index];
      return ret;
    endmethod
    `endif
		method Action fwd_from_exe (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
			fwd_data[index]<=tagged Present d;	
		endmethod
		method Action fwd_from_mem (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
			fwd_data[index]<=tagged Present d;	
		endmethod
    interface get_index = interface Get
      method ActionValue#(Bit#(TLog#(PRFDEPTH))) get;
        fwd_data[rg_index]<= tagged Absent;
        if(rg_index==2)
          rg_index<= 0;
        else
          rg_index<= rg_index+ 1;
        return rg_index;
      endmethod
    endinterface;
  endmodule

endpackage
