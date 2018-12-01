/*
Authors     : Vinod.G
Email       : g.vinod1993@gmail.com
Last Update : 27th November 2017 
See LICENSE for more details
Description:
TODO
*/

package fpu_convert_sp_dp_pipe;
import defined_types::*;
import UniqueWrappers::*;
`include "defined_parameters.bsv"
import FIFOF        :: *;

typedef struct{
    Bit#(1) sign;
    Bit#(8) exponent;
    Bit#(23) mantissa;
    Bit#(3) rounding_mode;
    Bit#(5) flags;
} Input_data_type deriving (Bits,Eq);

interface Ifc_fpu_convert_sp_dp_pipe;
    method Action _start(Bit#(1) sign, Bit#(8) exponent, Bit#(23) mantissa, Bit#(3) rounding_mode, Bit#(5) flags);
    method ActionValue#(Floating_output#(64)) get_result();    
endinterface

function Bit#(m) zeroExtendLSB(Bit#(n) value)
    provisos(Add#(a__, n, m));

    Bit#(m) resp = 0;
    resp[valueOf(m)-1:valueOf(m)-valueOf(n)] = value;
    return resp;
endfunction

function Bit#(69) floatDouble(Bit#(1) sign, Bit#(8) exponent, Bit#(23) mantissa, Bit#(3) rounding_mode, Bit#(5) flags);
    Bit#(5) exception = 0;
    if (flags[3]==1)
        return {5'b0,sign,63'b0};
    else if (flags[2] == 1 || flags[0] == 1) begin
        exception[4] = flags[0];
        return {exception,1'b0,11'd-1,1'b1,51'b0};
    end
    else if (flags[1] == 1)
        return {exception,sign,11'd-1,52'd0}; 
    else begin
        if(flags[4]==0) begin   //Normal numbers
            Bit#(11) expo = zeroExtend(exponent) + 'd896;
            Bit#(52) mant = zeroExtendLSB(mantissa);
            return {exception,sign,expo,mant};
        end
        else begin  //Denormal numbers
            let lv_zeros = countZerosMSB(mantissa);
            Bit#(11) expo = 'd896 - zeroExtend(pack(lv_zeros));   //What if lv_zeros is so much?
            Bit#(52) man = zeroExtendLSB(mantissa << lv_zeros);
            man = man<<1;  //To throw off implicit bit
            return {exception,sign, expo, man};
        end
    end
endfunction


module mkfpu_convert_sp_dp_pipe(Ifc_fpu_convert_sp_dp_pipe);
    //FIFOF#(Input_data_type) ff_input <- mkFIFOF();
    FIFOF#(Floating_output#(64)) ff_output <- mkFIFOF();

    
    method Action _start(Bit#(1) sign, Bit#(8) exponent, Bit#(23) mantissa, Bit#(3) rounding_mode, Bit#(5) flags);
        let x = floatDouble(sign , exponent , mantissa,rounding_mode,flags); 
        let op = Floating_output{
                                          final_result : x[63:0],
                                          fflags       : x[68:64]
                                        };
        ff_output.enq(op);
    endmethod

    method ActionValue#(Floating_output#(64 )) get_result();
        let  ff_final = ff_output.first ; ff_output.deq ;
        return ff_final ;
    endmethod

endmodule

/*
module mkTb(Empty);

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


	Reg#(Bit#(32)) rg_clock <-mkReg(0);
    //Reg#(Bit#(64)) wr_operand1 <- mkReg(64'h3721795900000000);
    //Reg#(Bit#(64)) wr_operand1 <- mkReg(64'h372741b800000000);
      Reg#(Bit#(64)) wr_operand1 <- mkReg(64'hffffffff7f814000);
    //Reg#(Bit#(64)) rg__operand1ut1 <- mkReg(64'h019000000000000);

	//Ifc_fpu_convert_dp_sp cvt <- mkfpu_convert_dp_sp;

	rule rl_clock;
        rg_clock<=rg_clock+1;
        if(rg_clock=='d60) begin
    	    $finish(0);
        end
	endrule
        
	rule give__operand1ut(rg_clock==2);
           let {man1,man2,man3}     <- getMant64.func(wr_operand1, 0,0);
           let {exp1,exp2,exp3}     <- getExp64.func(wr_operand1, 0,0);
           let {x1,x2,x3}           <- condFlags64.func(tuple2(man1,exp1),tuple2(0,0),tuple2(0,0));
           `ifdef verbose $display("sign: %b exponent : %b mantissa : %b",wr_operand1[63],exp1,man1); `endif
           `ifdef verbose $display("exponent: %d",exp1); `endif
          let x <- cvt._start(wr_operand1[63],exp1,man1,3'b011,x1);
		  `ifdef verbose $display("Output= %h fflags %h" , x.final_result,x.fflags,$time); `endif
    endrule

    
endmodule
*/
endpackage
