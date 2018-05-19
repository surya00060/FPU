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
package stage3;
  import common_types::*;
  `include "common_params.bsv"
	import TxRx:: *;

  import alu::*;
  import fwding1 ::*;

  interface Ifc_stage3;
		interface RXe#(PIPE2) rx_in;
		//interface TXe#(IE_IMEM_type) tx_out;
  endinterface

  module mkstage3(Ifc_stage3);
		RX#(PIPE2) rx <-mkRX;								// receive from the decode stage
//		TX#(IE_IMEM_type) tx <-mkTX;							// send to the memory stage;

    Reg#(Tuple2#(Flush_type, Bit#(VADDR))) check_npc <- mkReg(tuple2(None, 0));
    Ifc_fwding fwding <- mkfwding();

    rule execute_operation;
      let {optypes, opdata, metadata } = rx.u.first;
      let {op1, op2, op3, op4}=opdata;
      `ifdef bpu
        let {rd, word32, memaccess, fn, funct3, pred, epochs, trap}=metadata;
      `else
        let {rd, word32, memaccess, fn, funct3, epochs, trap}=metadata;
      `endif
      `ifdef spfpu
        let { rs1addr, rs2addr, rs3addr, rs1type, rs2type, rs3type, instrtype}=optypes;
      `else
        let { rs1addr, rs2addr, instrtype}=optypes;
      `endif
      Bit#(VADDR) pc = (instrtype==MEMORY || instrtype==JALR)?truncate(op1):truncate(op3);
      
      let rs1<- fwding.read_rs1(op1, truncate(rs1addr) );
      let rs2<- fwding.read_rs2(op2, truncate(rs2addr) );
      `ifdef spfpu
        let rs3<- fwding.read_rs3(rs3addr, op4, rs3type);
      `else
        let x4=op4;
      `endif
      if(trap matches tagged None)begin
        let {spec_res, expected_npc}=check_npc;
        if(spec_res==CheckNPC && pc!=expected_npc `ifdef bpu || spec_res==Mispredict `endif )begin
            // generate flush here
        end
        else begin
          if(rs1 matches tagged Present .x1 &&& rs2 matches tagged Present .x2 
                                      `ifdef spfpu &&& rs3 matches tagged Present .x4 `endif )begin
            let {cmtype, out, addr, trap1, spec} = fn_alu(fn, x1, x2, op3, truncate(x4), instrtype, 
                                           funct3, pc, memaccess, word32 `ifdef bpu ,pred `endif );
            // previous instruction was a branch or jump. Need to capture the next pc value to
            // ensure prediction was correct or not.
            check_npc<= tuple2(spec, addr);
          end
        end
      end
      rx.u.deq;
    endrule
		interface rx_in = rx.e;
  endmodule
endpackage
