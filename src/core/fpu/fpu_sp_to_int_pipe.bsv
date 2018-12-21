/*
Authors     : Vinod.G
Email       : g.vinod1993@gmail.com
Last Update : 27th November 2017
Description :
TODO
*/

package fpu_sp_to_int_pipe;
import defined_types::*;
import UniqueWrappers::*;
import FIFOF        :: *;
`include "defined_parameters.bsv"
interface Ifc_fpu_sp_to_int_pipe;
	method Action _start(Bit#(1) lv_sign,Bit#(8) lv_exponent, Bit#(23) lv_mantissa,  bit convert_unsigned, bit convert_long, Bit#(3) rounding_mode, Bit#(5) flags);
    method ActionValue#(Floating_output#(`Reg_width)) get_result();
endinterface

typedef struct{
    Bit#(1) lv_sign;
    Bit#(8) lv_exponent; 
    Bit#(23) lv_mantissa;
    bit convert_unsigned; 
    bit convert_long; 
    Bit#(3) rounding_mode;
    Bit#(5) flags;
}Input_data_type  deriving (Bits,Eq);


typedef struct{
    Bit#(1)     lv_sign;
    Bit#(8)     lv_exponent; 
    Bit#(23)    lv_mantissa;
    bit         convert_unsigned; 
    bit         convert_long; 
    Bit#(3)     rounding_mode;
    bit         lv_overflow;
    bit         lv_inexact;
    bit         lv_invalid;
    bit         lv_infinity;
    Bit#(`Reg_width) final_result;
    Int#(8) lv_original_exponent;
    Bool to_round;
    Bool rmm ;
    Bool rdn ;
    Bool rup ;
}Stage1_data_type  deriving (Bits,Eq);


module mkfpu_sp_to_int_pipe(Ifc_fpu_sp_to_int_pipe);

    FIFOF#(Input_data_type) ff_input <- mkFIFOF();
    FIFOF#(Stage1_data_type) ff_stage1 <- mkFIFOF();
    FIFOF#(Floating_output#(`Reg_width)) ff_out <- mkFIFOF();

    rule rl_stage1;
        
        let ff_input_pipe = ff_input.first ; ff_input.deq ;
        
        let lv_sign = ff_input_pipe.lv_sign ;
        let lv_exponent = ff_input_pipe.lv_exponent ;
        let lv_mantissa = ff_input_pipe.lv_mantissa ;
        let convert_long = ff_input_pipe.convert_long ;
        let convert_unsigned = ff_input_pipe.convert_unsigned ;
        let rounding_mode = ff_input_pipe.rounding_mode ;
        let flags =  ff_input_pipe.flags ;


        bit lv_overflow = 0;
		bit lv_zero = flags[3];
		bit lv_infinity = flags[1];
        bit lv_invalid = flags[0] | flags[2];
        bit lv_denormal = flags[4];
        bit lv_manzero = |lv_mantissa;
        bit lv_inexact = 0;
        Bool to_round = False;
        Bool rne = (rounding_mode == 3'b000);
        Bool rtz = (rounding_mode == 3'b001);
        Bool rdn = (rounding_mode == 3'b010);
        Bool rup = (rounding_mode == 3'b011);
        Bool rmm = (rounding_mode == 3'b100);
        Bit#(8) lv_exp = lv_exponent;
        `ifdef verbose $display("sign = %b exponent = %h mantissa = %h zero_flag = %b invalid_flag = %b inifnity: %b denormal %b", lv_sign, lv_exponent, lv_mantissa, lv_zero, lv_invalid, lv_infinity,lv_denormal); `endif
		Int#(8) lv_original_exponent = unpack(lv_exp - 127);  // removing the bias
        `ifdef verbose $display("lv_original_exponent : %d flags: %b",lv_original_exponent,flags);`endif
        Bit#(`Reg_width) final_result = 0;
		Bit#(TAdd#(23, `Reg_width)) final_man = {'0,1'b1,lv_mantissa};
       if(lv_zero == 1)
           final_result = 0;
       else if(lv_denormal == 1 || (lv_original_exponent <= -1 && (lv_infinity|lv_invalid) == 0)) begin
         if(lv_sign==1 && convert_unsigned==1 && ((lv_original_exponent==-1 && (rmm||(rne && lv_manzero==1))) || (lv_original_exponent<=-1 &&rdn)))
             lv_invalid = 1;
         else
           lv_inexact = 1;
           if(lv_sign == 0 && rup)
               final_result = 1;
           else if(rdn && lv_sign == 1 && convert_unsigned == 0)
               final_result = '1;
           else if(lv_original_exponent == -1 && (rmm||(rne && lv_manzero == 1)))begin
               if(lv_sign == 0)
                   final_result = 1;
               else if(convert_unsigned == 0)
                   final_result = '1;
               else
                   final_result = 0;
           end
           else
               final_result = 0;
       end
        else if(convert_long == 0) begin         //FCVT.W.S FCVT.WU.S
            if(convert_unsigned == 0) begin //FCVT.W.S
                Bit#(31) all_ones = '1;
                if(lv_infinity == 1 || lv_invalid == 1) begin
                   final_result = (lv_sign==1) ?(lv_invalid==1? zeroExtend(all_ones) : signExtend(32'h80000000)) : zeroExtend(all_ones); 
                end
               else if(lv_original_exponent < 'd31) begin
                   final_man = final_man << lv_original_exponent;
                   Bit#(32) y = final_man[54:23];
                   final_result = signExtend(y);
                   lv_mantissa = final_man[22:0];
                   to_round = True;
               end
               else if(lv_original_exponent >= 'd31) begin
                   `ifdef verbose $display("Overflow");`endif
                  // lv_overflow = 1;
                    lv_invalid = 1;
                   if(lv_sign == 0)
                    final_result = zeroExtend(all_ones);
                   else begin
                       if(lv_original_exponent == 'd31 && lv_manzero == 0)
                           lv_invalid = 0 ;        //Since we are exactly representing the number? 
                    final_result = signExtend(32'h80000000);
                   end
               end
            end
            else begin     //FCVT.WU.S
               Bit#(32) all_ones = '1;
               if(lv_infinity == 1 || lv_invalid == 1)
                   final_result = (lv_sign==1) ? (lv_invalid==1? signExtend(all_ones) : '0) : signExtend(all_ones); 
               else if(lv_original_exponent < 'd32) begin
                   final_man = final_man << lv_original_exponent;
                   Bit#(32) y = final_man[54:23];
                   final_result = signExtend(y);
                   lv_mantissa = final_man[22:0];
                   to_round = True;
               end
               else if(lv_original_exponent >= 'd32) begin
                   `ifdef verbose $display("Overflow");`endif
                   //lv_overflow = 1;
                     lv_invalid = 1;
                   if(lv_sign == 0)
                    final_result = signExtend(all_ones);
                   else
                    final_result = '0;
               end
            end
        end
        else begin
            if(convert_unsigned == 0) begin //FCVT.L.S
                Bit#(63) all_ones = '1;
               if(lv_infinity == 1 || lv_invalid == 1)
                   final_result = (lv_sign==1) ?(lv_invalid==1? zeroExtend(all_ones) : signExtend(64'h8000000000000000)) : zeroExtend(all_ones); 
               else if(lv_original_exponent < 'd63) begin
                   final_man = final_man << lv_original_exponent;
                   `ifdef verbose $display("final_man : %b",final_man);`endif
                   Bit#(64) y = zeroExtend(final_man[86:23]);
                   final_result = y;
                   lv_mantissa = final_man[22:0];
                   to_round = True;
               end
               else if(lv_original_exponent >= 'd63) begin
                   `ifdef verbose $display("Overflow");`endif
                   //lv_overflow = 1;
                   lv_invalid = 1;
                   if(lv_sign == 0)
                    final_result = zeroExtend(all_ones);
                   else begin
                       if(lv_original_exponent == 'd63 && lv_manzero == 0 )
                           lv_invalid = 0;  //Since we are exactly representing the input number
                    final_result = signExtend(64'h8000000000000000);
                   end
               end
            end
            else begin     //FCVT.LU.S
               Bit#(64) all_ones = '1;
               if(lv_infinity == 1 || lv_invalid == 1)
                   final_result = (lv_sign==1) ? (lv_invalid==1? signExtend(all_ones) : '0) : signExtend(all_ones); 
               else if(lv_original_exponent < 'd64) begin
                   final_man = final_man << lv_original_exponent;
                   Bit#(64) y = zeroExtend(final_man[86:23]);
                   final_result = y;
                   lv_mantissa = final_man[22:0];
                   to_round = True;
               end
               else if(lv_original_exponent >= 'd64) begin
                   `ifdef verbose $display("Overflow");`endif
                   //lv_overflow = 1;
                     lv_invalid = 1;
                   if(lv_sign == 0)
                    final_result = signExtend(all_ones);
                   else
                    final_result = '0;
               end
            end

        end

        let ff_stage1_pipe = Stage1_data_type{
                                                    lv_sign             : lv_sign ,
                                                    lv_exponent         : lv_exponent ,
                                                    lv_mantissa         : lv_mantissa ,
                                                    convert_unsigned    : convert_unsigned ,
                                                    convert_long        :  convert_long ,
                                                    rounding_mode       : rounding_mode ,
                                                    lv_overflow         :   lv_overflow ,
                                                    lv_inexact          :    lv_inexact ,
                                                    lv_invalid          :    lv_invalid ,
                                                    lv_infinity         :   lv_infinity ,
                                                    final_result        :  final_result ,
                                                    to_round            : to_round,
                                                    rmm                 : rmm ,   
                                                    rdn                 : rdn ,
                                                    rup                 : rup ,
                                                    lv_original_exponent: lv_original_exponent
                                            };
        ff_stage1.enq(ff_stage1_pipe);

    endrule

    rule rl_stage2;

        let ff_stage1_pipe = ff_stage1.first ; ff_stage1.deq ;

        let lv_sign             = ff_stage1_pipe.lv_sign ;
        let lv_exponent         = ff_stage1_pipe.lv_exponent ;
        let lv_mantissa         = ff_stage1_pipe.lv_mantissa ;
        let convert_unsigned    = ff_stage1_pipe.convert_unsigned ;
        let convert_long        = ff_stage1_pipe. convert_long ;
        let rounding_mode       = ff_stage1_pipe.rounding_mode ;
        let lv_overflow         = ff_stage1_pipe.  lv_overflow ;
        let lv_inexact          = ff_stage1_pipe.   lv_inexact ;
        let lv_invalid          = ff_stage1_pipe.   lv_invalid ;
        let lv_infinity         = ff_stage1_pipe.  lv_infinity ;
        let final_result        = ff_stage1_pipe. final_result ;
        let lv_original_exponent= ff_stage1_pipe.lv_original_exponent ;
        let to_round            = ff_stage1_pipe.to_round;
        let rmm                 = ff_stage1_pipe.rmm ;                
        let rdn                 = ff_stage1_pipe.rdn ;
        let rup                 = ff_stage1_pipe.rup ;

		bit lv_guard = lv_mantissa[22];	        //MSB of the already shifted mantissa is guard bit
    	bit lv_round = lv_mantissa[21];	        //next bit is round bit
    	bit lv_sticky = |(lv_mantissa<<2);		//remaining bits determine the sticky bit
	    bit lv_round_up = 0;
    	bit lv_inexact1 = lv_guard | lv_round | lv_sticky;
        if(to_round) begin
	    if(rounding_mode == 'b000) 		lv_round_up = lv_guard & (final_result[0] | lv_round | lv_sticky);	//Round to nearest ties to even
	    else if(rmm) lv_round_up = lv_guard; //& (lv_round | lv_sticky | ~lv_sign);			//Round to nearest ties to max magnitude
	    else if(rdn) lv_round_up = lv_inexact1 & (lv_sign);								//Round down to -infinity
	    else if(rup) lv_round_up = lv_inexact1 & (~lv_sign);								//Round up to +infinity
        lv_inexact = lv_inexact | lv_inexact1;
        if(lv_round_up == 1) begin //Should set the overflow flag here right? 
                lv_invalid = 1;
				if(convert_long == 0 && convert_unsigned == 0 && lv_original_exponent == 30 && final_result[30:0] == '1 && lv_sign == 0)  //Overflow..  Beyond representable number after rounding
                        final_result = 64'h7fffffff;
                else if(convert_long == 0 && convert_unsigned == 1 && lv_original_exponent == 31 && final_result[31:0] == '1 && lv_sign == 0)
                    final_result = 64'hffffffffffffffff;
                else if(convert_long == 1 && convert_unsigned == 0 && lv_original_exponent == 62 && final_result[62:0] == '1 && lv_sign == 0)  //Overflow..  Beyond representable number after rounding
                        final_result = 64'h7fffffffffffffff;
                else if(convert_long == 1 && convert_unsigned == 1 && lv_original_exponent == 63 && final_result[63:0] == '1 && lv_sign == 0)
                    final_result = 64'hffffffffffffffff;                
                else begin
                lv_invalid = 0;
                final_result = final_result + 1;
                if(convert_long == 0 && final_result[31]==1)
                    final_result = signExtend(final_result[31:0]);
                end
        end
        `ifdef verbose $display("rounding_mode == %b",rounding_mode);`endif
		`ifdef verbose $display("round_up = %b", lv_round_up);`endif

			if(convert_unsigned == 0 && lv_sign == 1)begin		//Negating the output if floating point number is negative and converted to signed word/long
				final_result = ~final_result + 1;
                if(convert_long == 0 && final_result[31] == 1)
                    final_result = signExtend(final_result[31:0]);
				`ifdef verbose $display("Negating output final_result : %b", final_result);`endif
			end
            else if(convert_unsigned == 1 && lv_sign == 1) begin
				final_result = 0;
                lv_invalid = 1;
            end
		end
        if((lv_invalid|lv_infinity) == 1) begin  //What about Quiet NaN?? What does the Spec Say?
            lv_overflow = 0;
            lv_inexact = 0;
        end
        Bit#(5) fflags={lv_invalid|lv_infinity,1'b0,lv_overflow,1'b0,lv_inexact};
		let f = Floating_output{
										final_result: final_result,
                                        fflags: fflags};
        ff_out.enq(f);
    endrule

	method Action _start(Bit#(1) lv_sign,Bit#(8) lv_exponent, Bit#(23) lv_mantissa,  bit convert_unsigned, bit convert_long, Bit#(3) rounding_mode, Bit#(5) flags);
		let ff_input_pipe = Input_data_type{
                                                lv_sign : lv_sign ,
                                                lv_exponent : lv_exponent ,
                                                lv_mantissa : lv_mantissa ,
                                                convert_long : convert_long ,
                                                convert_unsigned : convert_unsigned ,
                                                rounding_mode : rounding_mode ,
                                                flags : flags 
        };
        ff_input.enq( ff_input_pipe );

    endmethod

    method ActionValue#(Floating_output#(`Reg_width)) get_result();
        let  ff_final = ff_out.first ; ff_out.deq ;
        return ff_final ;
    endmethod

endmodule

/*module mkTb(Empty);

    function Tuple3#(Bit#(5), Bit#(5), Bit#(5)) condFlags (Tuple2#(Bit#(m), Bit#(e)) x, Tuple2#(Bit#(m), Bit#(e)) y, Tuple2#(Bit#(m),Bit#(e)) z);
        let s = valueOf(m);
        let man1  = tpl_1(x);
        let expo1 = tpl_2(x);
        let man2  = tpl_1(y);
        let expo2 = tpl_2(y);
        let man3  = tpl_1(z);
        let expo3 = tpl_2(z);
        Bit#(5) flags1, flags2,flags3;
        Bool expZ1 = (expo1 == 0);
        Bool manZ1 = (man1  == 0);
        Bool expO1 = (expo1 == '1);
        Bool manO1 = (man1  == '1);
        Bool topB1 = (man1[s-1] == 1);
        Bool expZ2 = (expo2 == 0);
        Bool manZ2 = (man2  == 0);
        Bool expO2 = (expo2 == '1);
        Bool manO2 = (man2  == '1);
        Bool topB2 = (man2[s-1] == 1 && man2 !=0);
        Bool expZ3 = (expo3 == 0);
        Bool manZ3 = (man3  == 0);
        Bool expO3 = (expo3 == '1);
        Bool manO3 = (man3  == '1);
        Bool topB3 = (man3[s-1] == 1 && man3 !=0);
        flags1 = {pack(expZ1 && !manZ1),pack(manZ1 && expZ1),pack(expO1 && topB1),pack(expO1 && manZ1),pack(expO1 && !topB1 && !manZ1)}; //Denormal, isZero, QNaN, Infinity, SNaN
        flags2 = {pack(expZ2 && !manZ2),pack(manZ2 && expZ2),pack(expO2 && topB2),pack(expO2 && manZ2),pack(expO2 && !topB2 && !manZ2)}; //Denormal, isZero, QNaN, Infinity, SNaN
        flags3 = {pack(expZ3 && !manZ3),pack(manZ3 && expZ3),pack(expO3 && topB3),pack(expO3 && manZ3),pack(expO3 && !topB3 && !manZ3)}; //Denormal, isZero, QNaN, Infinity, SNaN
        return tuple3(flags1,flags2,flags3);
    endfunction

    function Tuple3#(Bit#(m),Bit#(m), Bit#(m)) getMantissa (Bit#(n) op1, Bit#(n) op2, Bit#(n) op3)
        provisos(Add#(TAdd#(m,1),e,n),
                 Add#(7,a__,e)
                );
        let expo = valueOf(e);
        let man  = valueOf(m);
        return tuple3(op1[man-1:0],op2[man-1:0],op3[man-1:0]);
    endfunction

    function Tuple3#(Bit#(e), Bit#(e), Bit#(e)) getExp (Bit#(n) op1, Bit#(n) op2, Bit#(n) op3)
        provisos(Add#(TAdd#(m,1),e,n),
                 Add#(7,a__,e)
                );
        let inp = valueOf(n);
        let man  = valueOf(m);
        return tuple3(op1[inp-2:man], op2[inp-2:man], op3[inp-2:man]);
    endfunction

    function Bool isNaNBox(Bit#(64) op);
        return (op[63:32]=='1);
    endfunction

    function Tuple3#(Bit#(32),Bit#(32),Bit#(32)) setCanNaN (Bit#(64) op1, Bit#(64) op2, Bit#(64) op3);
        return tuple3(isNaNBox(op1)? truncate(op1) : 32'h7fc00000, isNaNBox(op2)? truncate(op2) : 32'h7fc00000, isNaNBox(op3)? truncate(op3) : 32'h7fc00000);
    endfunction
Wrapper3#(Tuple2#(Bit#(23), Bit#(8)),Tuple2#(Bit#(23), Bit#(8)), Tuple2#(Bit#(23), Bit#(8)),  Tuple3#(Bit#(5),Bit#(5),Bit#(5)))    condFlags32     <- mkUniqueWrapper3(condFlags);
    Wrapper3#(Tuple2#(Bit#(52), Bit#(11)),Tuple2#(Bit#(52), Bit#(11)),Tuple2#(Bit#(52), Bit#(11)), Tuple3#(Bit#(5),Bit#(5),Bit#(5)))   condFlags64     <- mkUniqueWrapper3(condFlags);
    Wrapper3#(Bit#(32),Bit#(32),Bit#(32),Tuple3#(Bit#(23),Bit#(23),Bit#(23)))                                                          getMant32       <- mkUniqueWrapper3(getMantissa);
    Wrapper3#(Bit#(32),Bit#(32),Bit#(32),Tuple3#(Bit#(8),Bit#(8),Bit#(8)))                                                             getExp32        <- mkUniqueWrapper3(getExp);
    Wrapper3#(Bit#(64),Bit#(64),Bit#(64),Tuple3#(Bit#(52),Bit#(52),Bit#(52)))                                                          getMant64       <- mkUniqueWrapper3(getMantissa);
    Wrapper3#(Bit#(64),Bit#(64),Bit#(64),Tuple3#(Bit#(11),Bit#(11),Bit#(11)))                                                          getExp64        <- mkUniqueWrapper3(getExp);
    Wrapper3#(Bit#(64),Bit#(64),Bit#(64),Tuple3#(Bit#(32),Bit#(32),Bit#(32)))                                                          setCanonicalNaN <- mkUniqueWrapper3(setCanNaN);


 	Ifc_fpu_sp_to_int_pipe converter <- mkfpu_sp_to_int_pipe();
 	Reg#(Bit#(32)) state_clock <- mkReg(0);
 	   Reg#(Bit#(32)) wr_operand1 <- mkReg('hbf7f0000);
 	   //Reg#(Bit#(32)) wr_operand1 <- mkReg('hbf214efa);
 	   //Reg#(Bit#(32)) wr_operand1 <- mkReg('h91c82527);
    
 	rule state_clock_count;
 		state_clock <= state_clock + 1;
 		if(state_clock == 'd5) $finish;
 	endrule

 	rule give_input(state_clock == 'd1);
 	//	let {op1,op2,op3} <- setCanonicalNaN.func(wr_operand1,'0,'0);
			let {man1,man2,man3}   <- getMant32.func(wr_operand1, 0,0);
            let {exp1,exp2,exp3}   <- getExp32.func(wr_operand1, 0,0);
            let {flags1,flags2,flags3} <- condFlags32.func(tuple2(man1,exp1),tuple2(man2,exp2),tuple2(0,0));
            let sign1 = wr_operand1[31];
`ifdef verbose $display("input %b %b %b given at %0d", sign1, exp1, man1, state_clock);`endif
 		let x <- converter._start(sign1,exp1,man1, 1, 1, 3'b010,flags1);
 `ifdef verbose $display("output : %h fflags : %h",x.final_result,x.fflags); `endif
 	endrule


 endmodule*/
endpackage