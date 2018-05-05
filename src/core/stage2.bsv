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
		interface RXe#(IF_ID_type) rx_in;
    (*always_ready*)
		interface TXe#(PIPE2) tx_out;
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
		RX#(IF_ID_type) rx <-mkRX;
		TX#(PIPE2) tx <-mkTX;
      
    let verbosity = `VERBOSITY ;
    Wire#(CSRtoDecode) wr_csrs <-mkWire();
		Reg#(Bit#(1)) eEpoch <-mkReg(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
    Reg#(Bool) rg_stall <- mkReg(False);

    rule decode_and_fetch(!rg_stall);
	    let pc=rx.u.first.program_counter;
	    let inst=rx.u.first.instruction;
	    let npc=rx.u.first.nextpc; // TODO get rid of this
	    let pred=rx.u.first.prediction;
	    let epochs=rx.u.first.epochs;
      let err=rx.u.first.accesserr_pagefault;
      if({eEpoch, wEpoch}!=epochs)begin
        rx.u.deq;
      end
      else begin
        let {opdecode, meta, trap} = decoder_func(inst, pc, err, wr_csrs);
        `ifdef spfpu
          let {rs1addr, rs2addr, rd, rs3addr, rs1type, rs2type, rs3type}=opdecode;
        `else
          let {rs1addr, rs2addr, rd, rs1type, rs2type} = opdecode;
        `endif

        `ifdef RV64
          let {fn, instrType, memaccess, imm, funct3, word32}=meta;
        `else
          let {fn, instrType, memaccess, imm, funct3}=meta;
        `endif

        if(instrType==SYSTEM_INSTR)
          rg_stall<= True;

        let {rs1, rs2 `ifdef spfpu , rs3 `endif }<-registerfile.opaddress(rs1addr, rs1type, rs2addr, 
            rs2type, pc, imm `ifdef spfpu , rs3addr, rs3type `endif );

        `ifdef spfpu
          OpTypes t1 =tuple7(rs1addr, rs2addr, rs3addr, rs1type, rs2type, rs3type, instrType);
          OpData t2 =tuple4(rs1, rs2, rs3, pc);
        `else
          OpTypes t1 =tuple5(rs1addr, rs2addr, rs1type, rs2type, instrType);
          OpData t2 =tuple4(rs1, rs2, imm, pc);
        `endif

        MetaData t3 = tuple7(rd, word32, memaccess, fn, funct3, pred, epochs);

        tx.u.enq(tuple3(t1, t2, t3));
        rx.u.deq; 
      end
    endrule

		method tx_out=tx.e;
		method rx_in=rx.e;
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
