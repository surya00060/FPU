/*
Authors     : Vinod.G, Arjun Menon, Aditya Govardhan
Email       : g.vinod1993@gmail.com, c.arjunmenon@gmail.com
Last Update : 27th November 2017
See LICENSE for more details
Description:
TODO
*/


package fpu_compare_min_max_32;
import defined_types::*;
`include "defined_parameters.bsv"
import FIFOF        :: *;

interface Ifc_fpu_compare_min_max_32;
    method Action _start(Bit#(32) operand1,Bit#(32) operand2, Bit#(3) which_cmp_instr,bit cmp_or_min_max,Tuple2#(Bit#(5),Bit#(5)) condFlags);
    method ActionValue#(Floating_output#(32)) get_result();
endinterface
    
//(*noinline*)
function Bit#(2) fn_comparator(bit sign1, Bit#(8) exponent1, Bit#(23) mantissa1, bit sign2, Bit#(8) exponent2, Bit#(23) mantissa2);

	Bit#(2) magnitude;					//01 means inp2's magnitude is greater than inp1's magnitude
												//10 means inp1's magnitude is greater than inp2's magnitude
												//11 means inp2's magnitude is equal to inp1's magnitude
	if(exponent1<exponent2)
		magnitude= 2'b01;
	else if(exponent1==exponent2)
	begin
		if(mantissa1<mantissa2)
			magnitude = 2'b01;
		else if(mantissa1==mantissa2)
			magnitude = 2'b11;
		else magnitude = 2'b10;
	end
	else
		magnitude = 2'b10;

	if(sign1==0) begin
		if(sign2==1)
			return 2'b10;
		else 
			return magnitude;
	end
	else begin
		if(sign2==1)
			return {magnitude[0],magnitude[1]};
		else
			return 2'b01;
	end
endfunction

typedef struct {
    Bit#(32) operand1 ;
    Bit#(32) operand2 ;
    Bit#(3)  which_cmp_instr ;
    bit cmp_or_min_max  ;
    Tuple2#(Bit#(5) , Bit#(5) ) condFlags ;
} Input_data_type deriving (Bits , Eq);

module mkfpu_compare_min_max_32(Ifc_fpu_compare_min_max_32);

    FIFOF#(Input_data_type) ff_input <- mkFIFOF();
    FIFOF#(Floating_output#(32)) ff_out <- mkFIFOF();

    rule rl_stage1;

        let ff_input_pipe = ff_input.first ; ff_input.deq ;

        let operand1 =  ff_input_pipe.operand1 ;     
        let operand2 =  ff_input_pipe.operand2 ;     
        let which_cmp_instr = ff_input_pipe.which_cmp_instr ;
        let cmp_or_min_max =  ff_input_pipe.cmp_or_min_max ;
        let condFlags      = ff_input_pipe.condFlags ;
      

        let fPMAN = 23 ;
        let fPEXP = 8  ;
        let fPINP = 32 ;
        let flags1 = tpl_1(condFlags);
        let flags2 = tpl_2(condFlags);
        let sign1 = operand1[fPINP-1];
        let sign2 = operand2[fPINP-1];
        Bit#(8) exponent1 = operand1[fPINP-2:fPMAN];
        Bit#(8) exponent2 = operand2[fPINP-2:fPMAN];
        Bit#(23) mantissa1 = operand1[fPMAN-1:0];
        Bit#(23) mantissa2 = operand2[fPMAN-1:0];
          Bit#(5) lv_exception = 0;
  
          Bit#(32) lv_result = 0;
         bit lv_invalid = 0;
          bit lv_zero = 0;
        `ifdef verbose $display("operand1 : %h operand2: %h",operand1,operand2);  `endif
  
        bit lv_op1_is_qNan=flags1[2];
        bit lv_op2_is_qNan=flags2[2];
         bit lv_op1_is_sNan=flags1[0];
        bit lv_op2_is_sNan=flags2[0];
        bit lv_op1_Nan=lv_op1_is_qNan | lv_op1_is_sNan;
        bit lv_op2_Nan=lv_op2_is_qNan | lv_op2_is_sNan;
        bit or_sign = sign1 | sign2;         //For accomodating spike's result ~ Spec is not clear!!!
           bit and_sign = sign1 & sign2;        //For accomodating spike's result ~ Spec is not clear!!!
  
           if(lv_op1_is_qNan==1 || lv_op1_is_sNan==1 || lv_op2_is_qNan==1 || lv_op2_is_sNan==1)
              lv_invalid=1;
  
          if(flags1[3]==1 && flags2[3]==1)
              lv_zero = 1;
      
          Bit#(2) lv_compare_result = fn_comparator(sign1,exponent1,mantissa1,sign2,exponent2,mantissa2);
              `ifdef verbose $display("lv_compare_result : %b sign1 %b exponent1 %b mantissa1 %b sign2 %b exponent2 %b mantissa2 %b",lv_compare_result,sign1,exponent1,mantissa1,sign2,exponent2,mantissa2); `endif
              if(cmp_or_min_max=='b0) begin						//checking if compare instruction
              if(lv_invalid==1)begin
                  if(which_cmp_instr!=3'b010)
                      lv_exception[4] = 1; // Invalid
                  else if((flags1[0] | flags2[0])==1)
                      lv_exception[4] = 1;
                          if((lv_op1_is_sNan | lv_op2_is_sNan)==1)
                              lv_result=0;
                          if(((lv_op1_is_qNan | lv_op2_is_qNan)==1) && which_cmp_instr!=3'b010)
                              lv_result=0;
              end
                  else if(which_cmp_instr==3'b010) begin				//FEQ.D, FEQ.S
                      `ifdef verbose $display("FEQ"); `endif
                      if(lv_compare_result==2'b11 || lv_zero==1)	//checking if op1=op2
                          lv_result[0]=1;					//writing result
                  end
                  else if(which_cmp_instr==3'b001) begin				//FLT.D, FLT.S
                      `ifdef verbose $display("FLT lv_compare_result %b", lv_compare_result); `endif
                      if(lv_compare_result==2'b01 && lv_zero==0)	//checking if op1<op2
                                                  //Also, if op1 is -0 and op2 is +0, lv_result[0] should be zero. lv_compare_is_zero takes care of that
                          lv_result[0]=1;					//writing result
                  end
                  else if(which_cmp_instr==3'b000) begin				//FLE.D, FLE.S
                      //`ifdef verbose $display("FLE"); `endif
                      if(lv_compare_result[0]==1'b1 || lv_zero==1)	//checking if op1<=op2; since less than is 01 and equal to is 11, enough to check LSB
                                                  //Also, if op1 is +0 and op2 is -0, lv_result[0] should be zero. lv_compare_is_zero takes care of that
                          lv_result[0]=1;					//writing result
                  end
              end
              else begin
                  /* According to IEEE 754-201x ratification Spec, they have introduced two operations called MinNum MaxNum, with a slightly different semantics with respect to NaN and (+0,-0) comparisons and the RISC-V spec chose to accomodate that, that change has been made here*/
                  Bit#(8) exp_all_ones = '1;
                  Bit#(TSub#(23,1)) man_all_zeros = '0;
                      if(lv_op1_is_sNan==1 || lv_op2_is_sNan==1)
                          lv_exception[4] = 1;
                  if((lv_op1_Nan&lv_op2_Nan)==1) begin
                         lv_result = {1'b0, exp_all_ones,1'b1, man_all_zeros};
                  end
                  else if(lv_op1_Nan==1) begin
                          lv_result=operand2;
                          //lv_exception[4] = 1;
                  end
                  else if(lv_op2_Nan==1) begin
                          lv_result=operand1;
                          //lv_exception[4] = 1;
                  end
                  else if((which_cmp_instr[0]==0 && lv_zero==1))	//FMIN.D, FMIN.S, and operands are 0
                          lv_result= {or_sign,truncate(operand2)};
                  else if((which_cmp_instr[0]==1 && lv_zero==1))	//FMAX.D, FMAX.S and operands are 0
                          lv_result= {and_sign,truncate(operand1)};
                  else if((which_cmp_instr[0]==0 && lv_compare_result==2'b01) || (which_cmp_instr[0]==1 && lv_compare_result==2'b10))	//FMIN.D, FMIN.S, FMAX.D, FMAX.S
                          lv_result= operand1;
                  else // in the alternate case of the above or if both the inputs are 0s (irrespective of +0 or -0 the output should be the first operand) or if both the inputs are the same magnitude.
                  lv_result= operand2;
              end
              let f = Floating_output{
                                                       final_result	: lv_result,
                                                       fflags	  	: lv_exception
                                                      };
              ff_out.enq(f);
    endrule

    method Action _start(Bit#(32) operand1,Bit#(32) operand2, Bit#(3) which_cmp_instr,bit cmp_or_min_max,Tuple2#(Bit#(5),Bit#(5)) condFlags);
        
        let x = Input_data_type{
                                operand1 : operand1 ,
                                operand2 : operand2 ,
                                which_cmp_instr : which_cmp_instr ,
                                cmp_or_min_max :  cmp_or_min_max ,
                                condFlags       : condFlags 
                                };
        ff_input.enq(x); 
    endmethod
    
    
    method ActionValue#(Floating_output#(32 )) get_result();
        let  ff_final = ff_out.first ; ff_out.deq ;
        return ff_final ;
    endmethod
		  	
  endmodule






/*
   module mkTb(Empty);
   Reg#(Bit#(32)) rg_operand1 <- mkReg(32'h00000000);
   Reg#(Bit#(32)) rg_operand2 <- mkReg(32'h0c4e2044);
   Reg#(Bit#(32)) rg_clock <- mkReg(0);
   Ifc_fpu_compare_min_max_32#(32,23,8) inst <- mkfpu_compare_min_max_32();
 
   rule rg_clock_cnt;
     rg_clock <= rg_clock + 1;
   endrule

   rule rl_start_1(rg_clock=='d0);
     `ifdef verbose $display("Giving Inputs to the compare module"); `endif
     inst._start(rg_operand1, rg_operand2, 3'b000, 0);
   endrule

   rule rl_display_result;
     let abc = inst.result_();
  //   inst._deque_buffer();
     `ifdef verbose $display("Final Result = %h", abc.final_result); `endif
   endrule

   rule end_clock(rg_clock == 'd10);
    $finish(0);
   endrule
  endmodule 
*/
endpackage
