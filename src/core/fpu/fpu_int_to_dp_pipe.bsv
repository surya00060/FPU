/*
Authors     : Vinod.G
Email       : g.vinod1993@gmail.com
Last Update : 27th November 2017
See LICENSE for more details
Description:
TODO
*/
package fpu_int_to_dp_pipe;
import defined_types ::*;
import UniqueWrappers::*;
import FIFOF        :: *;
`include "defined_parameters.bsv"	
//TODO Rework and optimize
function Bit#(m) zeroExtendLSB(Bit#(n) value)
    provisos(Add#(a__, n, m));

    Bit#(m) resp = 0;
    resp[valueOf(m)-1:valueOf(m)-valueOf(n)] = value;
    return resp;
endfunction

interface Ifc_fpu_int_to_dp_pipe;
    method Action _start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
    method ActionValue#(Floating_output#(64)) get_result();    
endinterface
    

typedef struct{
    Bit#(64) inp_int;
    Bit#(1) unsigned_bit; 
    Bit#(1) long;
    Bit#(3) rounding_mode;
}Input_data_type  deriving (Bits,Eq);

typedef struct{
    Bit#(78) res ;
}Stage1_data_type deriving (Bits,Eq);

    function Bit#(69) roundFunc(Bit#(64) unrounded, Bit#(11) expo, Bit#(3) rounding_mode);
           bit guard  = unrounded[10];
           bit round  = unrounded[9];
           bit sticky = 0;
           bit sign = unrounded[63];
           Bit#(11) local_expo = expo;
           Bit#(9) sticky_check = unrounded[8:0];  
           if(sticky_check != '0)
               sticky = 1;
           bit inexact  = (guard | round | sticky);
           bit lv_roundup = 0;
           Bit#(54) lv_man = {2'b0,unrounded[62:11]};
           if(rounding_mode == 'b000) 
		        lv_roundup = guard & (unrounded[11] | round | sticky);
	       else if (rounding_mode == 'b100)
		        lv_roundup = guard; //& (round | sticky | ~sign);
	       else if (rounding_mode == 'b011)
		        lv_roundup = (guard | round | sticky) & (~sign);
	       else if (rounding_mode == 'b010)
		        lv_roundup = (guard | round | sticky) & (sign);
           if(lv_roundup == 1)
           lv_man = lv_man + 1;
           if(lv_man[52] == 1) begin
               local_expo = local_expo + 1;
           end
           let fflags = {1'b0,1'b0,1'b0,1'b0,inexact};
        return {fflags,sign,local_expo,lv_man[51:0]};
    endfunction
  
 `ifdef fpu_hierarchical
  (*synthesize*)
 `endif
 module mkfpu_int_to_dp_pipe(Ifc_fpu_int_to_dp_pipe);

    //Wrapper3#(Bit#(32), Bit#(1), Bit#(3),Bit#(69)) fcvt_d_wwu <- mkUniqueWrapper3(fcvt_s_w_l);
    //Wrapper3#(Bit#(64), Bit#(1), Bit#(3),Bit#(69)) fcvt_d_llu <- mkUniqueWrapper3(fcvt_s_w_l);

    FIFOF#(Input_data_type) ff_input    <- mkFIFOF();
    FIFOF#(Floating_output#(64)) ff_out <- mkFIFOF();
    FIFOF#(Input_data_type) ff_pseudo   <- mkFIFOF();
    FIFOF#(Stage1_data_type) ff_stage1  <- mkFIFOF();

    Reg#(Bit#(32))          rg_rule_decider <- mkReg(0);
    Reg#(Bit#(32))          rg_rule_decider_2 <- mkReg(0);

    rule rl_case1_1( rg_rule_decider == 1 );
        let ff_input_pipe = ff_input.first ; ff_input.deq ;
        rg_rule_decider <= 0 ; 
        rg_rule_decider_2 <= 1 ;
        ff_pseudo.enq( ff_input_pipe );
    endrule

    rule rl_case1_2( rg_rule_decider_2== 1 );
        let ff_input_pipe = ff_pseudo.first ; ff_pseudo.deq ;
        let inp_int         = ff_input_pipe.inp_int ;
        let unsigned_bit    = ff_input_pipe.unsigned_bit ;
        let long            = ff_input_pipe.long ;
        let rounding_mode   = ff_input_pipe.rounding_mode ;
        Floating_output#(64) wr_final_out=?;
        rg_rule_decider_2 <= 0 ;
        wr_final_out = Floating_output{     final_result : 64'b0,
                                            fflags       : 5'b0
                                       };
        ff_out.enq(wr_final_out);
    endrule
    
    rule rl_case2_1( rg_rule_decider == 2 );

        let ff_input_pipe = ff_input.first ; ff_input.deq ;
        rg_rule_decider <= 0 ; 
        rg_rule_decider_2 <= 2 ;
        ff_pseudo.enq( ff_input_pipe );    
    endrule
    
    rule rl_case2( rg_rule_decider_2 == 2 );

        let ff_input_pipe   = ff_pseudo.first ; ff_pseudo.deq ;
        let inp_int         = ff_input_pipe.inp_int ;
        let unsigned_bit    = ff_input_pipe.unsigned_bit ;
        let long            = ff_input_pipe.long ;
        let rounding_mode   = ff_input_pipe.rounding_mode ;
        Floating_output#(64) wr_final_out=?;
        
        Bit#(32) inp32 = inp_int[31:0];
        `ifdef verbose $display("inp_int : %b",inp32); `endif
        Bool ubit = (unsigned_bit == 1);
        Bit#(1) lv_sign = ubit? 0 : inp32[31];
        Bool sbit = (lv_sign==1);
        Bit#(10) bias = '1;
        Bit#(11) expo = zeroExtend(bias) + 31;
        if(sbit)
            inp32 = ~inp32+1;
        Bit#(5) lv_zeros = truncate(pack(countZerosMSB(inp32)));
        inp32 = inp32 << lv_zeros;
        expo = expo - zeroExtend(lv_zeros);
        Bit#(52) mantissa = zeroExtendLSB(inp32[30:0]);
        Bit#(64) res = {lv_sign,expo,mantissa};
        rg_rule_decider_2 <= 0 ;
            wr_final_out = Floating_output {
                                              final_result : res,
                                              fflags       : 0
                                            };
        ff_out.enq(wr_final_out);
    endrule
    
    rule rl_case3_1( rg_rule_decider == 3 );
        let ff_input_pipe = ff_input.first ; ff_input.deq ;
        let inp_int         = ff_input_pipe.inp_int ;
        let unsigned_bit    = ff_input_pipe.unsigned_bit ;
        let long            = ff_input_pipe.long ;
        let rounding_mode   = ff_input_pipe.rounding_mode ;
        Floating_output#(64) wr_final_out=?;
    
        Bool ubit = (unsigned_bit == 1);
        Bit#(1) lv_sign = ubit? 0 : inp_int[63];
        Bool sbit = (lv_sign==1);
        Bit#(10) bias = '1;
        Bit#(11) expo = zeroExtend(bias) + 63;
        if(sbit)
            inp_int = ~inp_int + 1;
        Bit#(6) lv_zeros = truncate(pack(countZerosMSB(inp_int)));
        inp_int = inp_int << lv_zeros;
        expo = expo - zeroExtend(lv_zeros);
        Bit#(78) res = { {lv_sign,inp_int[62:0]},expo,rounding_mode } ;
        rg_rule_decider <= 0 ;
        rg_rule_decider_2 <= 3 ;
        let ff_stage1_pipe = Stage1_data_type { res :res } ;
        ff_stage1.enq( ff_stage1_pipe );
    endrule

    rule rl_case3_2( rg_rule_decider_2 == 3 );
        let ff_stage1_pipe = ff_stage1.first ; ff_stage1.deq ;
        let r = ff_stage1_pipe.res ;
        rg_rule_decider_2 <= 0 ;
        Bit#(69) res = roundFunc(r[77:14],r[13:3],r[2:0]);
        Floating_output#(64) wr_final_out=?;
        wr_final_out = Floating_output {
                                              final_result : res[63:0],
                                              fflags       : res[68:64]
                                        };

            ff_out.enq(wr_final_out);
    endrule
    
    method Action _start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
        if((inp_int == 0 && long==1) || (inp_int[31:0] == 0 && long == 0))
            rg_rule_decider <= 1 ;
        
        else if(long == 0)
            rg_rule_decider <= 2 ;
        
        else
            rg_rule_decider <= 3 ;
        
        let ff_input_pipe = Input_data_type{
                                            inp_int : inp_int ,
                                            unsigned_bit : unsigned_bit ,
                                            long : long ,
                                            rounding_mode : rounding_mode
                                            };
        ff_input.enq( ff_input_pipe );
    endmethod

    method ActionValue#(Floating_output#(64 )) get_result();
        let  ff_final = ff_out.first ; ff_out.deq ;
        return ff_final ;
    endmethod

endmodule

module mkTb(Empty);
    Reg#(Bit#(64)) rg_operand1<-mkReg(64'h039e781bab642be4); 
    //Reg#(Bit#(64)) rg_operand1<-mkReg(~(64'hfffffffffffff812)+1); 
    Reg#(Bit#(32)) rg_clock<-mkReg(0); 
    Ifc_fpu_int_to_dp_pipe itof <- mkfpu_int_to_dp_pipe();
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
