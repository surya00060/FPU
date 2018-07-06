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
    method ActionValue#(FwdType#(XLEN)) read_rs1 (Bit#(XLEN) rfvalue, Bit#(3) index);
    method ActionValue#(FwdType#(XLEN)) read_rs2 (Bit#(XLEN) rfvalue, Bit#(3) index);
    `ifdef spfpu                                              
    method ActionValue#(FwdType#(XLEN)) read_rs3 (Bit#(XLEN) rfvalue, Bit#(3) index);
    `endif

		method Action fwd_from_exe (Bit#(XLEN) d, Bit#(3) index);
		method Action fwd_from_mem (Bit#(XLEN) d, Bit#(3) index);
    method Action invalidate_index(Bit#(3) ind);
    method Action flush_mapping;
  endinterface

  (*synthesize*)
  (*conflict_free="fwd_from_exe, fwd_from_mem"*)
  (*conflict_free="fwd_from_exe, invalidate_index"*)
  (*conflict_free="invalidate_index, fwd_from_mem"*)
  module mkfwding(Ifc_fwding);
    let verbosity = `VERBOSITY ;
    Reg#(FwdType#(XLEN)) fwd_data [valueOf(PRFDEPTH)];
    for(Integer i=0;i<= 32;i=i+ 1)begin
      if(i<valueOf(PRFDEPTH))
        fwd_data[i]<- mkReg(tagged Absent); 
    end
    method Action flush_mapping;
      for(Integer i=0;i<valueOf(PRFDEPTH)-1;i=i+1)begin
        fwd_data[i]<= tagged Absent;
      end
    endmethod
    method ActionValue#(FwdType#(XLEN)) read_rs1 (Bit#(XLEN) rfvalue, Bit#(3) index);
      FwdType#(XLEN) ret= tagged Present rfvalue;
      if(index!=5)begin
        ret=fwd_data[index];
        if(verbosity>1)
          $display($time, "\tFWDING: Reading rs1 from prf. Data: %h index\
                %d",fwd_data[index[1:0]], index);
      end
      return ret;
    endmethod
    method ActionValue#(FwdType#(XLEN)) read_rs2 (Bit#(XLEN) rfvalue, Bit#(3) index);
      FwdType#(XLEN) ret= tagged Present rfvalue;
      if(index!=5)begin
        ret=fwd_data[index];
        if(verbosity>1)
          $display($time, "\tFWDING: Reading rs2 from prf. Data: %h index\
                %d",fwd_data[index[1:0]], index);
      end
      return ret;
    endmethod
    `ifdef spfpu                                                            
    method ActionValue#(FwdType#(XLEN)) read_rs3 (Bit#(XLEN) rfvalue, Bit#(3) index);
      FwdType#(XLEN) ret= tagged Present rfvalue;
      if(index!=5)
        ret=fwd_data[index];
      return ret;
    endmethod
    `endif
		method Action fwd_from_exe (Bit#(XLEN) d, Bit#(3) index);
      if(verbosity>1)
        $display($time, "\tFWDING: Got fwded data from exe. Data: %h index: %d", d, index);
			fwd_data[index]<=tagged Present d;	
		endmethod
		method Action fwd_from_mem (Bit#(XLEN) d, Bit#(3) index);
      if(verbosity>1)
        $display($time, "\tFWDING: Got fwded data from mem. Data: %h index: %d", d, index);
			fwd_data[index]<=tagged Present d;	
		endmethod
    method Action invalidate_index(Bit#(3) ind);
      fwd_data[ind]<= tagged Absent;
      if(verbosity>1)
        $display($time, "\tFWDING: Sending renamed index for rd: %d", ind);
    endmethod
  endmodule

endpackage
