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

  interface Ifc_stage3;
		interface RXe#(PIPE2) rx_in;
		//interface TXe#(IE_IMEM_type) tx_out;
  endinterface

  module mkstage3(Ifc_stage3);
		RX#(PIPE2) rx <-mkRX;								// receive ffrom the decode stage
//		TX#(IE_IMEM_type) tx <-mkTX;							// send to the memory stage;
    
    rule execute_operation;
      let {optypes, opdata, metadata } = rx.u.first;
      let {op1, op2, op3, op4}=opdata;
      let {rd, word32, memaccess, fn, funct3, pred, epochs}=metadata;
      `ifdef spfpu
        let { rs1addr, rs2addr, rs3addr, rs1type, rs2type, rs3type, instrtype}=optypes;
      `else
        let { rs1addr, rs2addr, instrtype}=optypes;
      `endif
      Bit#(VADDR) pc = (instrtype==MEMORY || instrtype==JALR)?truncate(op1):truncate(op3);

      // Put forwarding logic and stall logic here

      let reslt = fn_alu(fn, op1, op2, op3, truncate(op4), instrtype, funct3, word32);
      rx.u.deq;
    endrule
		interface rx_in = rx.e;
  endmodule
endpackage
