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
Details: This stage implements the execute stage of the pipe.

NOTE1: We are not passing the next pc through the pipe. Instead, if there is a branch or jal(r)
operation we store the next expected PC in a register. When the next instruction reaches the execute
stage we simply check if the this instruction's PC is what the previous branch/jalr expected. If not
we generate a flush. 
This mechanism will work in case a branch predictor is also present. However,  in this case we need
to know whether the next expected PC is PC+4 or a a different target address. 
Currently we store both the redirected pc and the pc+4 in a register and another variable which 
indicates which one of these 2 shuold the next instruction pc be compared to.

CATCH: In case we have a sequence:

beq, x0, x0, NEWLOC
wfi

Here once the branch is passes the execute stage,  the pipe will basically stall since the WFI will
not proceed ahead of the decode stage untill an interrupt has occurred. But since the branch is
taken the WFI should never be executed. This leads the pipe in a "hung" state.

There twooptions to solve this issue:
  
  1. in the decode stage as soon as you get a WFI encode and send a nop in the pipe. This will reach
  the execute stage and flush since the wfi pc != NEWLOC. Now suppose the branch was not taken, the 
  WFI needs to executed, but I have sent in a nop instruction from the decode. This instruction 
  needs to be dropped in the execute stage else it will corrupt my commit-trace. If might be 
  difficult to identify whether a "nop" is because of a WFI or is actually a required nop.

  2. Do not stall the WFI in the decode,  instead stall it in the writeback stage. Thus the wfi will
  propagate through the execute stage and get flushed. However,  the complexity here is to shift
  interrupt handling to the write-back stage. This is a huge overhead since the we capture all
  inetrrupts and majority of the exceptions in the decode stage. This logic will have to be
  replicated in the write-back stage as well. 

--------------------------------------------------------------------------------------------------
*/
package stage3;
  import common_types::*;
  `include "common_params.bsv"
	import TxRx:: *;
  import DReg::*;

  import alu::*;
  import fwding1 ::*;

  interface Ifc_stage3;
		interface RXe#(PIPE2) rx_in;
		interface TXe#(PIPE3) tx_out;
    method Action update_wEpoch;
    method Tuple2#(Bool, Bit#(VADDR)) flush_from_exe;
		method Action fwd_from_mem (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
    method ActionValue#(Bit#(TLog#(PRFDEPTH))) get_index();
    `ifdef bpu
  		method Maybe#(Training_data#(VADDR)) training_data;
    `endif
  endinterface

  module mkstage3(Ifc_stage3);
		RX#(PIPE2) rx <-mkRX;								// receive from the decode stage
		TX#(PIPE3) tx <-mkTX;							// send to the memory stage;

    `ifdef bpu
      Reg#(Tuple3#(Flush_type, Bit#(VADDR), Bit#(VADDR))) check_rpc <- mkReg(tuple3(None, 0, 0));
		  Reg#(Maybe#(Training_data#(VADDR))) wr_training_data <-mkDReg(tagged Invalid);
    `else
      Reg#(Tuple2#(Flush_type, Bit#(VADDR))) check_rpc <- mkReg(tuple2(None, 0));
    `endif
    Ifc_fwding fwding <- mkfwding();
		Reg#(Bit#(1)) eEpoch <-mkReg(0);
		Reg#(Bit#(1)) wEpoch <-mkReg(0);
    Wire#(Bool) wr_flush_from_exe <- mkDWire(False);
    Wire#(Bool) wr_flush_from_wb <- mkDWire(False);
    Wire#(Bit#(VADDR)) wr_redirect_pc <- mkDWire(0);

    rule flush_mapping(wr_flush_from_exe||wr_flush_from_wb);
      fwding.flush_mapping;
    endrule

    rule execute_operation;
      `ifdef simulate
        let {optypes, opdata, metadata, instruction } = rx.u.first;
      `else
        let {optypes, opdata, metadata } = rx.u.first;
      `endif
      let {op1, op2, op3, op4}=opdata;
      `ifdef bpu
        let {rd, word32, memaccess, fn, funct3, pred, epochs, trap}=metadata;
      `else
        let {rd, word32, memaccess, fn, funct3, epochs, trap}=metadata;
      `endif
      `ifdef spfpu
        let { rs1addr, rs2addr, rs3addr, rd_index, instrtype}=optypes;
      `else
        let { rs1addr, rs2addr, rd_index, instrtype}=optypes;
      `endif
      Bit#(VADDR) pc = (instrtype==MEMORY || instrtype==JALR)?truncate(op1):truncate(op3);
      
      Bool execute_instruction = ({eEpoch, wEpoch}==epochs);
      let rs1<- fwding.read_rs1(op1, truncate(rs1addr) );
      let rs2<- fwding.read_rs2(op2, truncate(rs2addr) );
      `ifdef spfpu
        let rs3<- fwding.read_rs3(rs3addr, op4);
      `else
        let x4=op4;
      `endif


      if(trap matches tagged None &&& execute_instruction)begin

        let {redirect_result, redirect_pc, npc}=check_rpc;
        if(redirect_result==CheckRPC && pc!=redirect_pc `ifdef bpu || 
                                                  redirect_result==CheckNPC && pc!=npc `endif )begin
            // generate flush here
          wr_flush_from_exe<=True;
          if(redirect_result==CheckRPC)
            wr_redirect_pc<= redirect_pc;
          else
            wr_redirect_pc<= npc;
          eEpoch<= ~eEpoch;
          rx.u.deq;
        end
        else begin
          if(rs1 matches tagged Present .x1 &&& rs2 matches tagged Present .x2 
                                      `ifdef spfpu &&& rs3 matches tagged Present .x4 `endif )begin
            rx.u.deq;
            let {cmtype, out, addr, trap1, redirect} = fn_alu(fn, x1, x2, op3, truncate(x4), 
                                instrtype, funct3, pc, memaccess, word32 `ifdef bpu ,pred `endif );
            // if previous instruction was a branch or jump. Need to capture the next pc value to
            // ensure prediction was correct or not.
            `ifdef bpu
              check_rpc<= tuple3(redirect, addr, (pc+ 4));
              if(instrtype==BRANCH)
                wr_training_data<= tagged Valid Training_data{pc:pc, branch_address:addr, state:?};// TODO  generate the right state bits
            `else
              check_rpc<= tuple2(redirect, addr);
            `endif



            `ifdef simulate
              tx.u.enq(tuple8(cmtype, out, rd, pc, truncate(addr), epochs[0], trap1, instruction));
            `else
              tx.u.enq(tuple7(cmtype, out, rd, pc, truncate(addr), epochs[0], trap1));
            `endif
            fwding.fwd_from_exe(out, rd_index);// Send index instead of rd TODO
          end
        end
      end
      else begin
        rx.u.deq; 
        if(execute_instruction) begin // trap has occurred on this instruction
          Bit#(XLEN) res=op1;
          `ifdef simulate
            tx.u.enq(tuple8(REGULAR, res, 0, pc, ?, epochs[0], trap, instruction));
          `else
            tx.u.enq(tuple7(REGULAR, res, 0, pc, ?, epochs[0], trap));
          `endif
        end
        // else you need to simply drop the execution since epochs have changed.
      end
    endrule
		interface rx_in = rx.e;
		interface tx_out = tx.e;
    method Action update_wEpoch;
      wEpoch<= ~wEpoch;
      wr_flush_from_wb<= True;
    endmethod
    method flush_from_exe=tuple2(wr_flush_from_exe, wr_redirect_pc);
		method Action fwd_from_mem (Bit#(XLEN) d, Bit#(TLog#(PRFDEPTH)) index);
      fwding.fwd_from_mem(d, index);
    endmethod
    method get_index() = fwding.get_index;
    `ifdef bpu
  		method training_data = wr_training_data;
    `endif
  endmodule
endpackage
