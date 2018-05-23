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
package common_types;
  `include "common_params.bsv"

  `ifdef RV64
  	typedef 64 XLEN;
  `else
    typedef 32 XLEN;
  `endif
	typedef 32 PADDR ;
  typedef 32 VADDR ;
	typedef Bit #(3)  Funct3;
  typedef 4 PRFDEPTH;

  //------ The follwing contain common tuples across the stages ------------- 
	typedef enum {ALU, MEMORY, BRANCH, JAL, JALR, SYSTEM_INSTR, 
      `ifdef spfpu FLOAT, `endif `ifdef muldiv MULDIV, `endif FENCE} Instruction_type 
      deriving(Bits, Eq, FShow); // the type of the decoded instruction.
	typedef enum {Load=0, Store=1 `ifdef atomic ,Atomic=2 `endif } Access_type 
                                                                        deriving (Bits, Eq, FShow);
  `ifdef bpu                                                                     
  	typedef enum {CheckNPC, CheckRPC, None} Flush_type deriving (Bits, Eq, FShow);
  `else
  	typedef enum {CheckRPC, None} Flush_type deriving (Bits, Eq, FShow);
  `endif
	typedef enum {`ifdef spfpu FloatingRF, `endif IntegerRF, PC} Op1type deriving(Bits, Eq, FShow);
	typedef enum {`ifdef spfpu FloatingRF, `endif IntegerRF, Immediate, Constant4} Op2type deriving(Bits, Eq, FShow);
  typedef enum {FRF, IRF} Op3type deriving(Bits, Eq, FShow);
  typedef enum {MEMORY, SYSTEM_INSTR, REGULAR} Commit_type deriving(Eq, Bits, FShow);
  typedef enum {Machine=3, `ifdef supervisor Supervisor=1, `endif User=0} Privilege_mode 
                                                                          deriving(Eq, Bits, FShow);
  // -------------------------------------------------------------------------------------

  // ------- The following typdefs are used to define the output from the decode stage -----
  // data structure of the fwding data structure
  typedef union tagged{
    Bit#(width) Present;
  	void Absent;
  } FwdType#(numeric type width) deriving(Bits,Eq,FShow);
  
  // Rdtype is not required here. The ALU or FPU unit can generate the rdtype at the respective
  // stage and use it commit or perform operand forwarding. This will reduce one bit propagation in
  // 2 stages and will also reduce decoding logic for the same.
  `ifdef spfpu
    // the following type is defined as:  rs1, rs2, rd, rs3, rs1type, rs2type, rs3type
    typedef Tuple7#(Bit#(5), Bit#(5), Bit#(5), Bit#(5), Op1type, Op2type, Op3type)
      OpDecode;
  `else
    typedef Tuple5#(Bit#(5), Bit#(5), Bit#(5), Op1type, Op2type) OpDecode;
  `endif

  `ifdef RV64
    // the following type is defined as: fn, InstrType, MemAccesstype, Immediate, funct3, wfi, word32 
    typedef Tuple7#(Bit#(4), Instruction_type, Access_type, Bit#(32), Bit#(3), Bool, Bool)
        DecodeMeta;
  `else
    // the following type is defined as: fn, InstrType, MemAccesstype, Immediate, funct3, wfi 
    typedef Tuple6#(Bit#(4), Instruction_type, Access_type, Bit#(32), Bit#(3), Bool)
        DecodeMeta;
  `endif

  // the following type is defined as: Operand Data, Decode Meta data,  Trap, resume_wfi
  `ifdef spfpu
    typedef Tuple5#(OpDecode, DecodeMeta, Trap_type, Bool, Op3type) DecodeOut ;
  `else
    typedef Tuple4#(OpDecode, DecodeMeta, Trap_type, Bool) DecodeOut ;
  `endif
  // ------------------------------------------------------------------------------------------

  `ifdef spfpu
    typedef Tuple7#(Bit#(XLEN), Bit#(XLEN), Bit#(XLEN), Bit#(2), Bit#(2), Bit#(2), Bit#(2)) Operands ;
  `else
    typedef Tuple5#(Bit#(XLEN), Bit#(XLEN), Bit#(2), Bit#(2), Bit#(2)) Operands ;
  `endif

  // define all tuples here
  typedef Tuple5#(Commit_type, Bit#(XLEN), Bit#(VADDR), Trap_type, Flush_type) ALU_OUT;
  
  typedef Tuple5#(Bit#(PADDR), Bit#(XLEN), Access_type, Bit#(2), Bit#(1)) MemoryRequest;
  typedef Tuple4#(Bit#(PADDR), Access_type, Bit#(2), Bit#(1)) CoreRequest;

  typedef Tuple3#(Bit#(5), Bool, Bit#(XLEN)) OpFwding;
  // rg_prv,  csr_mip, csr_mie, csr_mideleg, csr_misa, csr_counteren, rg_mie
  typedef Tuple7#(Privilege_mode, Bit#(12), Bit#(12), Bit#(12), Bit#(26), Bit#(3), 
                   Bit#(1)) CSRtoDecode;

  typedef Tuple5#(Privilege_mode, Bit#(XLEN), Bit#(32), Bit#(5), Bit#(XLEN)) DumpType;

	typedef enum {
		Inst_addr_misaligned=0,
		Inst_access_fault=1,
		Illegal_inst=2,
		Breakpoint=3,
		Load_addr_misaligned=4,
		Load_access_fault=5,
		Store_addr_misaligned=6,
		Store_access_fault=7,
		Ecall_from_user=8,
		Ecall_from_machine=11
    `ifdef supervisor
      , Inst_page_fault=12
      , Load_page_fault=13
      , Store_page_fault=15
    `endif
	} Exception_cause deriving (Bits,Eq,FShow);

	typedef enum{
		User_soft_int=0,
		Machine_soft_int=3,
		User_timer_int=4,
		Machine_timer_int=7,
		User_external_int=8,
		Machine_external_int=11
	} Interrupt_cause deriving (Bits,Eq,FShow);

	typedef union tagged{
	  Exception_cause Exception;
	  Interrupt_cause Interrupt;
	  void None;
	} Trap_type deriving(Bits,Eq,FShow);

typedef struct {
	Bit#(addr_width) pc;
	Bit#(addr_width) branch_address;
	Bit#(2) state;
  } Training_data#(numeric type addr_width) deriving (Bits, Eq);

// the data stucture for the pipeline FIFO between fetch and decode.
typedef struct{
	Bit#(VADDR) program_counter;
	Bit#(32) instruction;
	Bit#(VADDR) nextpc; // TODO get rid of this
	Bit#(2) prediction;
	Bit#(2) epochs;
  Bit#(2) accesserr_pagefault;
}IF_ID_type deriving (Bits,Eq);

// ---------- Tuples for the second Pipeline Stage -----------//
`ifdef spfpu
  typedef Tuple6#(Bit#(2),     // rs1addr
                Bit#(2),     // rs2addr
                Bit#(2),   // rs3addr ifdef spfpu
                Bit#(2),  // rd rename index 
                Op3type,  // rdtype
                Instruction_type // instr_type
                ) OpTypes;
`else
  typedef Tuple4#(Bit#(2),     // rs1addr
                  Bit#(2),     // rs2addr
                  Bit#(2),  // rd rename index 
                  Instruction_type // instr_type
                ) OpTypes;
`endif

`ifdef spfpu
  typedef Tuple4#( Bit#(XLEN),  // rs1_pc
                   Bit#(XLEN),  // rs2
                   Bit#(VADDR), // pc_rs1
                   Bit#(XLEN)   // rs3_imm
                 ) OpData;
`else
  typedef Tuple4#( Bit#(XLEN),  // rs1_pc
                   Bit#(XLEN),  // rs2
                   Bit#(VADDR), // pc_rs1 
                   Bit#(VADDR)  // imm
                 ) OpData;
`endif

`ifdef bpu
typedef Tuple8#(  Bit#(5),    // rd
                  Bool,       // word32
                  Access_type,  // mem_access
                  Bit#(4),  // fn
                  Bit#(3),  // funct3
                  Bit#(2),  // prediction
                  Bit#(2),    // epochs
                  Trap_type // trap type
                ) MetaData;
`else
typedef Tuple7#(  Bit#(5),    // rd
                  Bool,       // word32
                  Access_type,  // mem_access
                  Bit#(4),  // fn
                  Bit#(3),  // funct3
                  Bit#(2),    // epochs
                  Trap_type // trap type
                ) MetaData;
`endif
`ifdef simulate
  typedef Tuple4#( OpTypes, OpData, MetaData, Bit#(32)) PIPE2;
`else
  typedef Tuple3#( OpTypes, OpData, MetaData) PIPE2;
`endif
// -------------------------------------------------------------
// ---------- Tuples for the third Pipeline Stage -----------//
`ifdef spfpu
typedef Tuple8#(
  Commit_type,              // regular,  csr or memory
  Bit#(XLEN),               // rd value
  Bit#(5),                  // rd
  Bit#(VADDR),              // PC 
  Bit#(20),                 // CSR field
  Bit#(1),                  // epoch
  Trap_type,                // trap
  Op3type                   // rdtype
  ) ExecOut;
`else
typedef Tuple7#(
  Commit_type,              // regular,  csr or memory
  Bit#(XLEN),               // rd value
  Bit#(5),                  // rd
  Bit#(VADDR),              // PC 
  Bit#(20),                 // CSR field
  Bit#(1),                  // epoch
  Trap_type                // trap
  ) ExecOut;
`endif

`ifdef simulate
typedef Tuple2#(
  ExecOut, 
  Bit#(32)
  ) PIPE3;
`else
typedef ExecOut PIPE3;
`endif
// ----------------------------------------------------------//

endpackage
