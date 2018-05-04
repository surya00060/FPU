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
1.  This module decodes the instructions fetched from the previous stage and also fetches the 
    operands from the registerfile.
2.  If a csr operation is being decoded, then the next instruction is stalled untill the csr
    completes and commits the instruction.
--------------------------------------------------------------------------------------------------
*/
package stage2;
	/*=== package imports === */
	import FIFOF::*;
	import TxRx:: *;
	import DReg::*;
	/* ====================== */

	/* === project imports === */
	import registerfile::*;
	import decoder::*;
	import common_types::*;
	`include "common_params.bsv"
	/* ====================== */

	interface Ifc_stage2;
		method Action write_rd (Bit#(5)r, Bit#(XLEN) d `ifdef spfpu ,Operand_type rdtype `endif );
		/* ===== pipe connections ========= */
		//interface RXe#(IF_ID_type) rx_in;
		//interface TXe#(ID_IE_type) tx_out;
		/*================================= */
		`ifdef Debug
      method ActionValue#(Bit#(XLEN)) read_write_gprs(Bit#(5) r, Bit#(XLEN) data 
          `ifdef spfpu ,Op3type rfselect `endif );
		`endif
		method Action flush();
    method Action csrs (CSRtoDecode csr);
    method Action csr_updated (Bool upd);
		method Action update_eEpoch;
		method Action update_wEpoch;
	endinterface:Ifc_stage2

  module mkstage2(Ifc_stage2);

    Ifc_registerfile registerfile <-mkregisterfile();
      
    let verbosity = `VERBOSITY ;
    Wire#(CSRtoDecode) wr_csrs <-mkWire();
		Reg#(Bit#(1)) eEpoch <-mkReg(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
    Reg#(Bool) rg_stall <- mkReg(False);

    method Action csrs (CSRtoDecode csr);
      wr_csrs<= csr;
    endmethod
		method Action write_rd (Bit#(5)r, Bit#(XLEN) d `ifdef spfpu , Operand_type rdtype `endif )=
                                    registerfile.write_rd(r,d `ifdef spfpu ,rdtype `endif );

    // This method will get activated when there is a flush from the execute stage
		method Action update_eEpoch;
		  if(verbosity>1) 
        $display($time,"\tSTAGE2: updating eEpoch"); 
			eEpoch<=~eEpoch;
		endmethod
    // This method gets activated when there is a flush from the write-back stage.
		method Action update_wEpoch;
			if(verbosity>1)
        $display($time,"\tSTAGE2: updating wEpoch"); 
			wEpoch<=~wEpoch;
		endmethod
    method Action csr_updated (Bool upd) if(rg_stall);
      if(upd)
        rg_stall<= False;
    endmethod
  endmodule
endpackage
