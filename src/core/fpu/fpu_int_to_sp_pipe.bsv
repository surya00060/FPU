/*
Authors     : Vinod.G
Email       : g.vinod1993@gmail.com
Last Update : 27th November 2017
See LICENSE for more details
Description:
TODO
*/
package fpu_int_to_sp_pipe;

import defined_types ::*;
import UniqueWrappers::*;
import FIFOF        :: *;
`include "defined_parameters.bsv"	
interface Ifc_fpu_int_to_sp_pipe;
    method Action _start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
    method ActionValue#(Floating_output#(32)) get_result();
endinterface

typedef struct{
    Bit#(64) inp_int;
    Bit#(1) unsigned_bit; 
    Bit#(1) long;
    Bit#(3) rounding_mode;
}Input_data_type  deriving (Bits,Eq);

typedef struct{
    Bit#(43) res ;
}Stage1_data_type deriving ( Bits , Eq );

typedef struct{
    Bit#(75) res ;
}Stage1_64_data_type deriving ( Bits , Eq );

    function Bit#(37) roundFunc(Bit#(43) res );
           let rounding_mode = res[2:0] ;
           let expo          = res[10:3];
           let unrounded     = res[42:11]; 

           let nInd = 32 ;
           bit guard  = unrounded[nInd-25];
           bit round  = unrounded[nInd-26];
           bit sticky = 0;
           bit sign = unrounded[nInd-1];
           Bit#(8) local_expo = expo;
           Bit#(TSub#(32,26)) sticky_check = unrounded[nInd-27:0];  
           if(sticky_check != '0)
               sticky = 1;
           bit inexact  = (guard | round | sticky);
           bit lv_roundup = 0;
           Bit#(25) lv_man = {2'b0,unrounded[nInd-2:nInd-24]};
           if(rounding_mode == 'b000) 
		        lv_roundup = guard & (unrounded[nInd-24] | round | sticky);
	       else if (rounding_mode == 'b100)
		        lv_roundup = guard; //& (round | sticky | ~sign);
	       else if (rounding_mode == 'b011)
		        lv_roundup = (guard | round | sticky) & (~sign);
	       else if (rounding_mode == 'b010)
		        lv_roundup = (guard | round | sticky) & (sign);
           if(lv_roundup == 1)
           lv_man = lv_man + 1;
           if(lv_man[23] == 1) begin
               local_expo = local_expo + 1;
           end
           let fflags = {1'b0,1'b0,1'b0,1'b0,inexact};
        return {fflags,sign,local_expo,lv_man[22:0]};
    endfunction
   
    function Bit#(43) fcvt_s_w_l (Bit#(32) inp, Bit#(1) unsigned_bit, Bit#(3) rounding_mode);
           let nInd = 32;
           Bool ubit = (unsigned_bit == 1);
           Bit#(1) lv_sign = ubit? 0 : inp[nInd-1];
           Bool sbit = (lv_sign == 1);
           Bit#(7)  bias = '1;
           Bit#(8)  expo = zeroExtend(bias) + fromInteger(nInd-1);
           if(sbit)
               inp = ~inp + 1;
           Bit#(5) lv_zeros = truncate(pack(countZerosMSB(inp)));
           inp = inp << lv_zeros;
           expo = expo - zeroExtend(pack(lv_zeros));
           Bit#(TSub#(32,1)) inpS = inp[nInd-2:0];
           Bit#(32) inp_temp = {lv_sign,inpS};
           Bit#(43) res = { inp_temp , expo , rounding_mode }; 
           return res;
    endfunction

    function Bit#(37) roundFunc64(Bit#(75) res);
           let rounding_mode = res[2:0] ;
           let expo          = res[10:3];
           let unrounded     = res[74:11]; 

           let nInd = 64;
           bit guard  = unrounded[nInd-25];
           bit round  = unrounded[nInd-26];
           bit sticky = 0;
           bit sign = unrounded[nInd-1];
           Bit#(8) local_expo = expo;
           Bit#(TSub#(64,26)) sticky_check = unrounded[nInd-27:0];  
           if(sticky_check != '0)
               sticky = 1;
           bit inexact  = (guard | round | sticky);
           bit lv_roundup = 0;
           Bit#(25) lv_man = {2'b0,unrounded[nInd-2:nInd-24]};
           if(rounding_mode == 'b000) 
		        lv_roundup = guard & (unrounded[nInd-24] | round | sticky);
	       else if (rounding_mode == 'b100)
		        lv_roundup = guard; //& (round | sticky | ~sign);
	       else if (rounding_mode == 'b011)
		        lv_roundup = (guard | round | sticky) & (~sign);
	       else if (rounding_mode == 'b010)
		        lv_roundup = (guard | round | sticky) & (sign);
           if(lv_roundup == 1)
           lv_man = lv_man + 1;
           if(lv_man[23] == 1) begin
               local_expo = local_expo + 1;
           end
           let fflags = {1'b0,1'b0,1'b0,1'b0,inexact};
        return {fflags,sign,local_expo,lv_man[22:0]};
    endfunction
   
    function Bit#(75) fcvt_s_w_l_64 (Bit#(64) inp, Bit#(1) unsigned_bit, Bit#(3) rounding_mode);
           let nInd = 64;
           Bool ubit = (unsigned_bit == 1);
           Bit#(1) lv_sign = ubit? 0 : inp[nInd-1];
           Bool sbit = (lv_sign == 1);
           Bit#(7)  bias = '1;
           Bit#(8)  expo = zeroExtend(bias) + fromInteger(nInd-1);
           if(sbit)
               inp = ~inp + 1;
           Bit#(6) lv_zeros = truncate(pack(countZerosMSB(inp)));
           inp = inp << lv_zeros;
           expo = expo - zeroExtend(pack(lv_zeros));
           Bit#(TSub#(64,1)) inpS = inp[nInd-2:0];
           Bit#(64) inp_temp = {lv_sign,inpS};
           Bit#(75) res = { inp_temp , expo , rounding_mode }; 
           return res;
    endfunction

    
`ifdef fpu_hierarchical
(*synthesize*)
`endif
module mkfpu_int_to_sp_pipe(Ifc_fpu_int_to_sp_pipe);

    FIFOF#(Input_data_type ) ff_input   <- mkFIFOF();
    FIFOF#(Input_data_type ) ff_pseudo  <- mkFIFOF();
    FIFOF#(Stage1_data_type) ff_stage1  <- mkFIFOF();
    FIFOF#(Stage1_64_data_type) ff_stage1_64 <- mkFIFOF();
    FIFOF#(Floating_output#(32)) ff_out <- mkFIFOF();

    Reg#(Bit#(32)) rg_rule_decider <- mkReg(0);
    Reg#(Bit#(32)) rg_rule_decider_2 <- mkReg(0);

    //Wrapper3#(Bit#(32), Bit#(1), Bit#(3),Bit#(43)) fun1 <- mkUniqueWrapper3(fcvt_s_w_l);
    //Wrapper#( Bit#(43), Bit#(37)) fun2 <- mkUniqueWrapper(roundFunc);
    //Wrapper3#(Bit#(64), Bit#(1), Bit#(3),Bit#(37)) fcvt_s_llu <- mkUniqueWrapper3(fcvt_s_w_l);

        rule rl_case1( rg_rule_decider == 1 );
            let ff_input_pipe = ff_input.first ; ff_input.deq ;
            rg_rule_decider <= 0 ;
            rg_rule_decider_2 <= 1 ;
            ff_pseudo.enq(ff_input_pipe);
        endrule

        rule rl_case1_2( rg_rule_decider_2 == 1 );
            let ff_input_pipe = ff_pseudo.first ; ff_pseudo.deq ;
            let inp_int         = ff_input_pipe.inp_int ;
            let unsigned_bit    = ff_input_pipe.unsigned_bit ;
            let long            = ff_input_pipe.long ;
            let rounding_mode   = ff_input_pipe.rounding_mode ;

		    Floating_output#(32) wr_final_out=?;
            wr_final_out = Floating_output{ final_result : 32'b0,
                                            fflags       : 5'b0
                                           } ;
            rg_rule_decider_2<= 0;
            ff_out.enq(wr_final_out);
        endrule
        
        rule rl_case2( rg_rule_decider == 2 );
            let ff_input_pipe = ff_input.first ; ff_input.deq ;
            rg_rule_decider <= 0 ;
            rg_rule_decider_2 <= 1 ;
            ff_pseudo.enq(ff_input_pipe);
        endrule        
        
        rule rl_case2_2( rg_rule_decider_2 == 2 );
            let ff_input_pipe = ff_pseudo.first ; ff_pseudo.deq ;
            let inp_int         = ff_input_pipe.inp_int ;
            let unsigned_bit    = ff_input_pipe.unsigned_bit ;
            let long            = ff_input_pipe.long ;
            let rounding_mode   = ff_input_pipe.rounding_mode ;
		    Floating_output#(32) wr_final_out=?;
            Bit#(32) inp32 = truncate(inp_int);
            Bit#(1) lv_sign = inp32[31];
            Bit#(32) res = lv_sign==1? {1'b1,8'h9e,'0} : '0;
            wr_final_out = Floating_output{
                                        final_result : res,
                                        fflags        : 0
                                       };
            rg_rule_decider_2 <= 0;
            ff_out.enq(wr_final_out);
        endrule
        
        rule rl_case3( rg_rule_decider == 3 );
            let ff_input_pipe = ff_input.first ; ff_input.deq ;
            let inp_int         = ff_input_pipe.inp_int ;
            let unsigned_bit    = ff_input_pipe.unsigned_bit ;
            let long            = ff_input_pipe.long ;
            let rounding_mode   = ff_input_pipe.rounding_mode ;
            
            Bit#(32) inp32 = truncate(inp_int);
            Bit#(43) res = fcvt_s_w_l (inp32,unsigned_bit,rounding_mode);
            let ff_stage1_pipe = Stage1_data_type{ res : res };
            rg_rule_decider_2 <= 3 ;
            rg_rule_decider   <= 0 ; 
            ff_stage1.enq( ff_stage1_pipe );
        endrule
        
        rule rl_case3_2( rg_rule_decider_2 == 3 );
            let ff_stage1_pipe  = ff_stage1.first;
            ff_stage1.deq;
            Floating_output#(32) wr_final_out=?;
            Bit#(37) ressw = roundFunc(ff_stage1_pipe.res); 
            rg_rule_decider_2 <= 0 ;
            wr_final_out = Floating_output{
                                      final_result : (ressw[31:0]),
                                      fflags       : ressw[36:32]
                                       };
            ff_out.enq(wr_final_out);
        endrule

        rule rl_case4( rg_rule_decider == 4 );
            let ff_input_pipe = ff_input.first ; ff_input.deq ;
            let inp_int         = ff_input_pipe.inp_int ;
            let unsigned_bit    = ff_input_pipe.unsigned_bit ;
            let long            = ff_input_pipe.long ;
            let rounding_mode   = ff_input_pipe.rounding_mode ;
		    
            Bit#(32) inp32 = truncate(inp_int);
            Bit#(43) res = fcvt_s_w_l (inp32,unsigned_bit,rounding_mode);
            let ff_stage1_pipe = Stage1_data_type{ res : res };
            rg_rule_decider_2 <= 4 ;
            rg_rule_decider   <= 0 ; 
            ff_stage1.enq( ff_stage1_pipe );
        endrule

        rule rl_case4_2( rg_rule_decider_2 == 4 );
            let ff_stage1_pipe  = ff_stage1.first;
            ff_stage1.deq;
            Floating_output#(32) wr_final_out=?;
            Bit#(37) res = roundFunc(ff_stage1_pipe.res);
            rg_rule_decider_2 <= 0 ; 
            wr_final_out = Floating_output{
                                        final_result  : (res[31:0]),
                                        fflags        : res[36:32]
                                        };
            ff_out.enq(wr_final_out);
        endrule
        
        rule rl_case5( rg_rule_decider == 5 );
            let ff_input_pipe = ff_input.first ; ff_input.deq ;
            let inp_int         = ff_input_pipe.inp_int ;
            let unsigned_bit    = ff_input_pipe.unsigned_bit ;
            let long            = ff_input_pipe.long ;
            let rounding_mode   = ff_input_pipe.rounding_mode ;

            Bit#(75) res = fcvt_s_w_l_64(inp_int,unsigned_bit,rounding_mode);
            let ff_stage1_pipe = Stage1_64_data_type{ res : res };
            rg_rule_decider_2 <= 5 ;
            rg_rule_decider   <= 0 ; 
            ff_stage1_64.enq( ff_stage1_pipe );
        endrule

        rule rl_case5_2( rg_rule_decider_2 == 5 );
            let ff_stage1_pipe  = ff_stage1_64.first;
            ff_stage1_64.deq;
            Bit#(37) res = roundFunc64(ff_stage1_pipe.res);
            Floating_output#(32) wr_final_out=?;
            wr_final_out = Floating_output {
                                         final_result : res[31:0],
                                         fflags       : res[36:32]
                                        };
            ff_out.enq(wr_final_out);
        endrule


    method Action _start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
        
        if( ( inp_int == 0 && long == 1 ) || ( inp_int[31:0] == 0 && long == 0 ) )
            rg_rule_decider <= 1 ;
        else if ( long == 0 ) begin
            Bit#(32) inp32 = truncate(inp_int);
            Bit#(1) lv_sign = inp32[31];
            if( unsigned_bit == 0 ) begin
                if((inp32 & 'h7fffffff) == 0)
                    rg_rule_decider <= 2;
                else 
                    rg_rule_decider <= 3 ;
                end
            else
                rg_rule_decider <= 4 ;
        end
        else
            rg_rule_decider <=5 ;
        
        let ff_input_pipe = Input_data_type{
                                        inp_int : inp_int ,
                                        unsigned_bit : unsigned_bit ,
                                        long : long ,
                                        rounding_mode : rounding_mode
                                        };
        ff_input.enq( ff_input_pipe );

    endmethod

    method ActionValue#(Floating_output#(32)) get_result();
        let  ff_final = ff_out.first ; ff_out.deq ;
        return ff_final ;
    endmethod

endmodule

module mkTb(Empty);
   Reg#(Bit#(64)) rg_operand1<-mkReg(64'h039e781bab642be4); 
   //Reg#(Bit#(64)) rg_operand1<-mkReg(~(64'hfffffffffffff812)+1); 
   Reg#(Bit#(32)) rg_clock<-mkReg(0); 
   Ifc_fpu_int_to_sp_pipe itof <- mkfpu_int_to_sp_pipe();
   Reg#(Bit#(32)) rg_arbit <-mkReg(0);

   rule rl_clk_count;
      rg_clock<=rg_clock+1;
      if(rg_clock=='d25) $finish(0);
   endrule

   rule rl_start_1(rg_clock <='d10);
       `ifdef verbose $display("Giving inputs rg_operand 1 : %h through testbench",rg_operand1,$time); `endif
       $display("Giving inputs rg_operand 1 : %h through testbench %d ",rg_operand1,rg_clock);
       itof._start(zeroExtend(rg_operand1),1'b1,1'b0,3'b000);
   endrule

   rule rl_out ;
        let abc <- itof.get_result();
        `ifdef verbose $display("Final result= %h fflags= %h", abc.final_result, abc.fflags, $time); `endif
        $display("                          Final result= %h  %d", abc.final_result[31:0], rg_clock);
    endrule
       

endmodule
endpackage
