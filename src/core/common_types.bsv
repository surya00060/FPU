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

  //----------------------- TODO TO BE REVISED ---------------------------------------------------
  typedef 256 BTB_DEPTH;
  // cache related parameters //
	typedef 64 DCACHE_ADDR       ;
	typedef 4  DCACHE_BLOCK_SIZE ;
	typedef 8  DCACHE_WORD_SIZE  ;
  typedef 4   DCACHE_WAYS       ;
  typedef 512 DCACHE_SETS       ;
  typedef 4	  ICACHE_WAYS      ;
  typedef 8	  ICACHE_BLOCK_SIZE ;
  typedef 512 ICACHE_SETS       ;
  typedef 4	  ICACHE_WORD_SIZE  ;
  typedef 20  ICACHE_TAG_BITS   ;
  typedef 20  DCACHE_TAG_BITS   ;
  typedef enum {
		Load, Store, Execution} Translation_type deriving(Bits, Eq);

  `ifdef RV64
    typedef 2 Byte_offset;
  `else
    typedef 1 Byte_offset;
  `endif

  // performance_counter
  typedef 64 PERFMONITORS;
  typedef 12 OFFSET;
  typedef 8 ASID;	
  typedef 64 ADDR;



typedef struct{
    Bit#(PADDR) address;
    Bit#(8) burst_length; 
    Access_type ld_st;
    Bit#(2) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits 3 - 64-bits;
    Bit#(TMul#(DCACHE_BLOCK_SIZE,TMul#(DCACHE_WORD_SIZE,8))) data_line;
  } To_Memory_Write deriving(Bits,Eq);

typedef struct {
	Bit#(data_width) vaddr;
	Access_type  ld_st_atomic;
} DTLB_access#(numeric type data_width) deriving(Bits, Eq);
	
typedef struct {
	bit v;					//valid
	bit r;					//allow reads
	bit w;					//allow writes
	bit x;					//allow execute(instruction read)
	bit u;					//allow supervisor
	bit g;					//global page
	bit a;					//accessed already
	bit d;					//dirty
} TLB_permissions deriving(Bits, Eq, FShow);

typedef enum {
		PTW_ready, Handling_PTW, Wait_for_memory, PTW_done, Send_to_memory} PTW_state deriving(Bits, Eq);

typedef struct {
	Bit#(TSub#(paddr,page_size)) ppn;
	TLB_permissions 				tlb_perm;
	Bit#(asid_width)						asid;
	Bit#(2)										levels;
} To_TLB#(numeric type paddr, numeric type page_size, numeric type asid_width) deriving(Bits,Eq); 
  
typedef struct {
	Translation_type 	page_type;
	To_TLB#(paddr_width,page_offset,asid_width) 	tlb_packet;
} Response_PPN_TLB#(numeric type paddr_width, numeric type page_offset, numeric type asid_width) deriving (Bits,Eq);

typedef struct {
	Bool ptwdone;
	Translation_type 	page_type;
	Bit#(data_width) 	address;
} Request_PTE_memory#(numeric type data_width) deriving (Bits,Eq);

typedef struct {
	Trap_type exception;
	Bit#(data_width) address;	
	Bool 						 cacheable;
} From_TLB#(numeric type data_width) deriving (Bits, Eq);

typedef struct {
	Bit#(vaddr_width) rs1;
	Bit#(vaddr_width) rs2;
} Fence_VMA_type#(numeric type vaddr_width) deriving (Bits, Eq);

typedef enum {
	Store_pf, Load_pf, Instruction_pf, None} Pf_exception_type deriving (Bits, Eq);	
typedef struct {
	Translation_type 	page_type;
	Bit#(TSub#(vaddr_width,page_offset)) 	vpn;
} Request_PPN_PTW#(numeric type vaddr_width, numeric type page_offset) deriving (Bits,Eq);

  typedef struct{
    Bit#(addr_width) address;
    Bit#(8) burst_length; 
    Access_type ld_st;
	 Bit#(2) transfer_size;
  }To_Memory#(numeric type addr_width) deriving(Bits,Eq);

  typedef struct{
    Bit#(TMul#(word_size,8)) data_line;
    Bit#(1) bus_error;
	 Bool last_word;
  }From_Memory#(numeric type word_size) deriving(Bits,Eq);

// ----------------------------------------------------------------------------------------------
  `ifdef RV64
  	typedef 64 XLEN;
  `else
    typedef 32 XLEN;
  `endif
	typedef 32 PADDR ;
  typedef 39 VADDR ;
	typedef Bit #(3)  Funct3;
  typedef 4 PRFDEPTH;
  typedef 8 RAS_DEPTH;


  //------ The follwing contain common tuples across the stages ------------- 
	typedef enum {ALU, MEMORY, BRANCH, JAL, JALR, SYSTEM_INSTR, 
      `ifdef spfpu FLOAT, `endif `ifdef muldiv MULDIV, `endif FENCE} Instruction_type 
      deriving(Bits, Eq, FShow); // the type of the decoded instruction.
	typedef enum {Load=0, Store=1,  Fence=3 `ifdef atomic ,Atomic=2 `endif } Access_type 
                                                                        deriving (Bits, Eq, FShow);
  `ifdef bpu                                                                     
  	typedef enum {CheckNPC, CheckRPC, Fence, None} Flush_type deriving (Bits, Eq, FShow);
  `else
  	typedef enum {CheckRPC, Fence, None} Flush_type deriving (Bits, Eq, FShow);
  `endif
  typedef enum {Fence, Regular, None} Flush_type2 deriving (Bits, Eq, FShow);
	typedef enum {`ifdef spfpu FloatingRF, `endif IntegerRF, PC} Op1type deriving(Bits, Eq, FShow);
	typedef enum {`ifdef spfpu FloatingRF, `endif IntegerRF, Immediate, Constant4} Op2type deriving(Bits, Eq, FShow);
  typedef enum {FRF, IRF} Op3type deriving(Bits, Eq, FShow);
  typedef enum {MEMORY, SYSTEM_INSTR, REGULAR} Commit_type deriving(Eq, Bits, FShow);
  typedef enum {Machine=3, Supervisor=1, User=0} Privilege_mode 
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
    typedef Tuple6#(Bit#(XLEN), Bit#(XLEN), Bit#(XLEN), Bit#(2), Bit#(2), Bit#(2)) Operands ;
  `else
    typedef Tuple4#(Bit#(XLEN), Bit#(XLEN), Bit#(2), Bit#(2)) Operands ;
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
      , Inst_pagefault=12
      , Load_pagefault=13
      , Store_pagefault=15
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

  typedef struct{
  	Bit#(XLEN) address;
  	Bit#(XLEN) memory_data; // data to be written in the memory
  	Bit#(2) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits 3 - 64-bits;
  	Bit#(1) signextend; // whether the loaded value has to be signextended
  	Access_type mem_type; // STORE or AMO or LOAD or FENCE
  	`ifdef atomic Bit#(5) atomic_op; `endif
  	}Memrequest deriving(Bits,Eq,FShow);
  
  // the data stucture for the pipeline FIFO between fetch and decode.
  typedef struct{
  	Bit#(VADDR) program_counter;
  	Bit#(32) instruction;
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
  
  // for the following,  funct3 & fn is required for memory ops which will need both type of atomic
  // op and also the type of acess: byte, half-word, word,  double word.
  // TODO see if in the current technique this can be avoided.
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
  typedef Tuple3#(
    ExecOut,
    Bit#(2), 
    Bit#(32)
    ) PIPE3;
  `else
  // rd index also
  typedef Tuple2#(ExecOut, Bit#(2)) PIPE3;
  `endif
  // ----------------------------------------------------------//

  typedef struct {
  	Bit#(1)			mprv;
  	Bit#(1)			sum;
  	Bit#(1)			mxr;
  	Privilege_mode mpp;
  	Privilege_mode prv;
  } Chmod deriving(Bits, Eq);

  `ifdef spfpu
    typedef Tuple4#(Bit#(5), Bit#(XLEN), Bit#(TLog#(PRFDEPTH)), Op3type) CommitData;
  `else
    typedef Tuple3#(Bit#(5), Bit#(XLEN), Bit#(TLog#(PRFDEPTH))) CommitData;
  `endif

/*======= AXI4 master/slave numbers ======*/
typedef 0 Sdram_slave_num;
typedef  TAdd#(Sdram_slave_num	 ,`ifdef SDRAM		1 `else 0 `endif )		Sdram_cfg_slave_num;
typedef	TAdd#(Sdram_cfg_slave_num,`ifdef BOOTROM	1 `else 0 `endif )		BootRom_slave_num	;
typedef	TAdd#(BootRom_slave_num  ,`ifdef Debug		1 `else 0 `endif )		Debug_slave_num	;
typedef  TAdd#(Debug_slave_num	 , `ifdef TCMemory	1 `else 0 `endif )		TCM_slave_num;
typedef  TAdd#(TCM_slave_num	 ,`ifdef DMA			1 `else 0 `endif )	Dma_slave_num;
typedef  TAdd#(Dma_slave_num	  ,1 )		SlowPeripheral_slave_num;
typedef  TAdd#(SlowPeripheral_slave_num,`ifdef VME	1 `else 0 `endif )       VME_slave_num;
typedef  TAdd#(VME_slave_num,`ifdef FlexBus	1 `else 0 `endif )              FlexBus_slave_num;
typedef	TAdd#(FlexBus_slave_num,1)						 Num_Slaves;
typedef 0 Dmem_master_num;
typedef 1 Imem_master_num;
typedef TAdd#(Imem_master_num , `ifdef Debug 1 `else 0 `endif ) Debug_master_num;
typedef TAdd#(Debug_master_num, `ifdef DMA 1 `else 0 `endif ) DMA_master_num;
typedef TAdd#(DMA_master_num,1) Num_Masters;

/*=============================================================================== */
endpackage
