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
package stage4;
  import TxRx::*;
  import GetPut::*;
  import common_types::*;
  `include "common_params.bsv"

  import FIFO::*;
  import FIFOF::*;
  import csr::*;
  import csrfile::*;

  interface Ifc_stage4;
    interface RXe#(PIPE3) rx_in;
    interface Put#(Maybe#(Tuple3#(Bit#(XLEN), Bool, Access_type))) memory_response;
    interface Get#(CommitData) commit_rd;
    interface Get#(Tuple2#(Bit#(XLEN), Bit#(TLog#(PRFDEPTH)))) fwd_from_mem;
    method Tuple2#(Bool, Bit#(VADDR)) flush;
    method CSRtoDecode csrs_to_decode;
	  method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Bool csr_updated;
    method Bool interrupt;
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
    `ifdef RV64 method Bool inferred_xlen; `endif // False-32bit,  True-64bit 
		`ifdef supervisor
			method Bit#(XLEN) send_satp;
			method Chmod perm_to_TLB;
		`endif
    `ifdef spfpu
  		method Bit#(3) roundingmode;
    `endif
	  method Action set_external_interrupt(Tuple2#(Bool,Bool) ex_i);
  endinterface

  (*synthesize*)
  module mkstage4(Ifc_stage4);

    RX#(PIPE3) rx<-mkRX;
    Ifc_csr csr <- mkcsr();
    Wire#(Bool) wr_csr_updated <- mkDWire(False);

    // wire that captures the response coming from the external memory or cache.
    Wire#(Maybe#(Tuple3#(Bit#(XLEN), Bool,  Access_type))) wr_memory_response <- mkDWire(tagged Invalid);

    // wire that carriues the information for operand forwarding
    Wire#(Maybe#(Tuple2#(Bit#(XLEN), Bit#(TLog#(PRFDEPTH))))) wr_operand_fwding <- mkDWire(tagged
                                                                                            Invalid);

    // wire that carries the commit data that needs to be written to the integer register file.
    Wire#(Maybe#(CommitData)) wr_commit <- mkDWire(tagged Invalid);

    // wire which signals the entire pipe to be flushed.
    Wire#(Tuple2#(Bool, Bit#(VADDR))) wr_flush <- mkDWire(tuple2(False, ?));

    // the local epoch register
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);

    `ifdef simulate
      FIFO#(DumpType) dump_ff <- mkLFIFO;
      let prv=tpl_1(csr.csrs_to_decode);
    `endif

    Integer verbosity = `VERBOSITY;

    rule instruction_commit;
      `ifdef simulate
        let {execout, rdindex, inst}=rx.u.first;
      `else
        let {execout, rdindex}=rx.u.first;
      `endif
      `ifdef spfpu
        let {committype, rd, rdaddr, pc, csrfield, epoch, trap, rdtype}=execout;
      `else
        let {committype, rd, rdaddr, pc, csrfield, epoch, trap}=execout;
      `endif
      Bit#(VADDR) jump_address=0;
      Bool fl = False;
      // continue commit only if epochs match. Else deque the ex fifo
      if(trap matches tagged Interrupt .in)begin
        let newpc<-  csr.take_trap(trap, pc, ?);
        wr_flush<=tuple2(True, newpc);
        rg_epoch <= ~rg_epoch;
        rx.u.deq;
        if(verbosity!=0)
          $display($time, "\tSTAGE3: Received Interrupt: ", fshow(trap));
      end
      else if(rg_epoch==epoch)begin
        // in case of a flush also flip the local epoch register.
        // if instruction is of memory type then wait for response from memory
        if(trap matches tagged Exception .ex)begin
          jump_address<- csr.take_trap(trap, pc, truncate(rd));
          fl= True;
          rx.u.deq;
          if(verbosity!=0)
            $display($time, "\tSTAGE3: Received Exception: ", fshow(trap));
        end
        else if(committype == MEMORY) begin
          if (wr_memory_response matches tagged Valid .resp)begin
            let {data, err, access_type}=resp;
            if(!err)begin // no bus error
              wr_operand_fwding <= tagged Valid tuple2(data, rdindex);
              `ifdef spfpu
                wr_commit <= tagged Valid (tuple4(rdaddr, data, rdindex, rdtype));
              `else
                wr_commit <= tagged Valid (tuple3(rdaddr, data, rdindex));
              `endif
              `ifdef simulate 
                dump_ff.enq(tuple5(prv, zeroExtend(pc), inst, rdaddr, data));
              `endif
            end
            else begin
              if(verbosity!=0)
                $display($time, "\tSTAGE3: Received Exception from Memory: ", fshow(resp));
              if(access_type == Load)
                trap = tagged Exception Load_access_fault;
              else
                trap = tagged Exception Store_access_fault;
              jump_address<- csr.take_trap(trap, pc, truncate(rd));
              fl= True;
            end
            rx.u.deq;
          end
        end
        else if(committype == SYSTEM_INSTR)begin
          let {drain, newpc, dest}<-csr.system_instruction(csrfield[11:0], 
                                              csrfield[16:12], rd, csrfield[19:17]);
          jump_address=newpc;
          fl=drain;
          `ifdef simulate 
            dump_ff.enq(tuple5(prv, zeroExtend(pc), inst, rdaddr, dest));
          `endif
          `ifdef spfpu
            wr_commit <= tagged Valid (tuple4(rdaddr, dest, rdindex, rdtype));
          `else
            wr_commit <= tagged Valid (tuple3(rdaddr, dest, rdindex));
          `endif
          rx.u.deq;
        end
        else begin
          // in case of regular instruction simply update RF and forward the data.
          $display($time, "\tSTAGE3: Commiting PC: %h", pc);
          wr_operand_fwding <= tagged Valid tuple2(rd,  rdindex);
          `ifdef spfpu
            wr_commit <= tagged Valid (tuple4(rdaddr, rd, rdindex, rdtype));
          `else
            wr_commit <= tagged Valid (tuple3(rdaddr, rd, rdindex));
          `endif
          rx.u.deq;
          `ifdef simulate 
            dump_ff.enq(tuple5(prv, zeroExtend(pc), inst, rdaddr, rd));
          `endif
        end
        
        // if it is a branch/JAL_R instruction generate a flush signal to the pipe. 
        wr_flush<=tuple2(fl, jump_address);
        if(fl)begin
          rg_epoch <= ~rg_epoch;
        end
        if(fl || committype==SYSTEM_INSTR)
          wr_csr_updated<= True;

      end
      else begin
        if(verbosity!=0)
          $display($time, "\tSTAGE3: Dropping instruction");
        rx.u.deq;
      end
    endrule

    rule increment_instruction_counter(wr_commit matches tagged Valid .x);
      csr.incr_minstret;
    endrule

    interface  memory_response= interface Put
      method Action put (Maybe#(Tuple3#(Bit#(XLEN), Bool,  Access_type)) response);
        wr_memory_response <= response;
      endmethod
    endinterface;

    interface rx_in=rx.e;

    interface commit_rd = interface Get
      method ActionValue#(CommitData) get() if(wr_commit matches tagged Valid .data);
        return data;
      endmethod
    endinterface;

    interface fwd_from_mem = interface Get
      method ActionValue#(Tuple2#(Bit#(XLEN), Bit#(TLog#(PRFDEPTH)))) get 
                                                    if(wr_operand_fwding matches tagged Valid .data);
        return data;
      endmethod
    endinterface;

    method flush=wr_flush;
    method csrs_to_decode = csr.csrs_to_decode;
    method Bool csr_updated = wr_csr_updated;

	  method Action clint_msip(Bit#(1) intrpt);
      csr.clint_msip(intrpt);
    endmethod
		method Action clint_mtip(Bit#(1) intrpt);
      csr.clint_mtip(intrpt);
    endmethod
		method Action clint_mtime(Bit#(XLEN) c_mtime);
      csr.clint_mtime(c_mtime);
    endmethod
    `ifdef simulate
      interface dump = interface Get
        method ActionValue#(DumpType) get ;
          dump_ff.deq;
          return dump_ff.first;
        endmethod
      endinterface;
    `endif
    `ifdef RV64 method Bool inferred_xlen = csr.inferred_xlen; `endif // False-32bit,  True-64bit 
    method  interrupt=csr.interrupt;
		`ifdef supervisor
			method send_satp=csr.send_satp;
			method perm_to_TLB=csr.perm_to_TLB;
		`endif
    `ifdef spfpu
  		method roundingmode=csr.roundingmode;
    `endif
	  method Action set_external_interrupt(Tuple2#(Bool,Bool) ex_i)=csr.set_external_interrupt(ex_i);
  endmodule
endpackage
