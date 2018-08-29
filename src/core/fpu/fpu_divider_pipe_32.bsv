/*
Authors     : Surya.S , Vinod.G, Arjun Menon, Aditya Govardhan 
Email       : g.vinod1993@gmail.com, c.arjunmenon@gmail.com
Last Update : 27th November 2017
See LICENSE for more details
Description:
TODO
*/
package fpu_divider_pipe_32;
    import DReg::*;
    import defined_types::*;                              //contains typedef of exceptions which are generated here
    import integer_divider::*;               //divider module
    `include "defined_parameters.bsv"	
    import RegFile::*;
    import FIFOF        :: *;

    typedef struct{
    	Bit#(TAdd#(8,2)) exponent;                              
    	Bit#(TAdd#(23,1)) dividend;
    	Bit#(TAdd#(23,1)) divisor;
        bit sign;
    	bit invalid;
    	bit infinity;
    	bit dz;
    	bit zero;
		Bit#(3) rounding_mode;
    bit quiet_nan;
    } Stage1_type deriving (Bits,Eq);                   //Structure of 1st Stage of the pipeline

     typedef struct {
            Bit#(10) exponent;
            Bit#(1) sign;
            Bit#(1) infinity;
            Bit#(1) invalid;
            Bit#(1) dz;
            Bit#(1) zero;
            Bit#(3) rounding_mode;
            bit quiet_nan;
     } Stage2_type deriving (Bits,Eq);
     
     typedef struct {
        Bit#(27) lv_quotient;
        Bit#(28) lv_remainder;
        Bit#(10) exponent;
        Bit#(1) sign;
        Bit#(1) infinity;
        Bit#(1) invalid;
        Bit#(1) dz;
        Bit#(1) zero;
        Bit#(3) rounding_mode;
        bit quiet_nan;
    } Stage2_1_type deriving (Bits,Eq);

      typedef struct {
            Bit#(TAdd#(23,4)) lv_quotient;
            Bit#(TAdd#(23,5)) lv_remainder;
            Bit#(10) lv_exponent;
            Bit#(1) lv_sign;
            Bit#(1) lv_infinity;
            Bit#(1) lv_invalid;
            Bit#(1) lv_dz;
            bit lv_underflow;
            bit lv_overflow;
            bit lv_sticky;
            Bit#(1) lv_zero;
            Bit#(3) lv_rounding_mode;
            bit lv_quotient_is_subnormal;
            bit quiet_nan;
     } Stage3_type deriving (Bits,Eq);
     

    interface Ifc_fpu_divider_pipe_32;
	    method Action _start(Bit#(1) lv_sign, Bit#(23) lv_mantissa1, Bit#(8) lv_exponent1, Bit#(23) lv_mantissa2, Bit#(8) lv_exponent2, Bit#(3) rounding_mode, Tuple2#(Bit#(5),Bit#(5)) flags);
    	method Floating_output#(32) final_result_();				 // Output method
        method Action flush;
    endinterface

    
//(*synthesize*)
module mkfpu_divider_pipe_32(Ifc_fpu_divider_pipe_32)
       provisos(
                //Add#(TAdd#(23,8),1,32), // 23 -23 8=8 32 = 32
                //Add#(23,2,25), // 25 = 25
                //Add#(25,2,27), // 27 = 27
                //Add#(27,1,28), // 28 = 28
                //Add#(8,2,10), // 10 = 10
                //Add#(28,1,29), // 29 = 29
                //Add#(27,29,56), // 56= 56
                Add#(10,b__,23),
                Add#(TSub#(8,1),c__,23),
                //per request of bsc
                Add#(a__, 1, 10),
                Add#(d__, TLog#(TAdd#(1, TAdd#(8, c__))), 10),
                Add#(e__, TLog#(28), 10),
                Add#(1, f__, 25),
                Add#(g__, 1, 27),
				Add#(h__, TLog#(TAdd#(1, TAdd#(c__, 8))), 10),
                Add#(1, 10, TAdd#(8, i__))
               );

    Ifc_integer_divider#(27) int_div <- mkinteger_divider();    // instantiation of divider module

	//Wire#(Floating_output#(32))  wr_final_out <- mkWire();			// instantiation of output FIFO whose structure definition is given in riscv_types.bsv
    Wire#(Floating_output#(32)) wr_final_out <- mkWire();
	//Reg#(Stage1_type#(23,8)) rg_stage1    <- mkRegU();       // instantiation of Stage 1 FIFO
    //Reg#((Stage2_type#(10)))    rg_stage2    <- mkRegU();
    //Reg#((Stage3_type#(10,23))) rg_stage3 <- mkRegU();
    FIFOF#(Stage1_type)  ff_stage1    <-mkFIFOF();
    FIFOF#(Stage2_type)  ff_stage2    <-mkFIFOF();
    FIFOF#(Stage2_1_type)  ff_stage2_1    <-mkFIFOF();
    FIFOF#(Stage3_type)  ff_stage3    <-mkFIFOF();
    //Reg#(Div_states) rg_state_handler            <- mkReg(Begin);
    Wire#(Bool) wr_flush <- mkDWire(False);
    let fPINP 	= valueOf(32);
    let fPMAN 	= valueOf(23);
    let fPMAN5  = valueOf(28);
    let fPEXP 	= valueOf(8);
    let aCC   	= valueOf(56);

    (*mutually_exclusive = "rl_stage2,rl_stage3"*)  
    //This is the second stage of the pipe. Here the division of the two mantissas take place. Rest of the data are enqueued in another FIFO.
	rule rl_stage2 ;//(rg_state_handler == Stage1 && !wr_flush);
        let stage1 = ff_stage1.first ;ff_stage1.deq ;  
        int_div._inputs({stage1.divisor,3'd0},
	    				{stage1.dividend,3'd0}
	    				);
         //rg_state_handler <= Stage2;
        //`ifdef verbose $display("Dividing Op1: %h (%d) Op2: %h (%d)",{stage1_data.dividend,3'd0},{stage1_data.dividend,3'd0},{stage1_data.divisor,3'd0},{stage1_data.divisor,3'd0});  `endif
        //`ifdef verbose $display("dz : %b",stage1_data.dz); `endif
        let stage2 = Stage2_type { exponent : stage1.exponent,
                                     				sign     : stage1.sign,
                                     				infinity : stage1.infinity,
                                     				invalid  : stage1.invalid,
                                     				dz       : stage1.dz,
                                     				zero     : stage1.zero,
                                     				rounding_mode : stage1.rounding_mode,
                                            quiet_nan : stage1.quiet_nan
                                 };
	    ff_stage2.enq( stage2 );

	endrule

    rule rl_stage2_1;

        let int_out = int_div.result_();
        
        `ifdef verbose $display("Int Data %h", int_out); `endif
		Bit#(27) lv_quotient 	= int_out[fPMAN+3:0];	//Quotient from the integer divider
		Bit#(28) lv_remainder 	= int_out[aCC-1:fPMAN5]; //Remainder from the integer divider
        
        let stage2 = ff_stage2.first ; ff_stage2.deq;

        Bit#(10) lv_exponent 	    = stage2.exponent;
		Bit#(1)  lv_sign			= stage2.sign;
		Bit#(1)  lv_infinity    	= stage2.infinity;
		Bit#(1)  lv_invalid 		= stage2.invalid;
		Bit#(1)  lv_dz 				= stage2.dz;
		Bit#(1)  lv_zero 			= stage2.zero;
		Bit#(3)  lv_rounding_mode	= stage2.rounding_mode;
        let quiet_nan = stage2.quiet_nan;

        let stage2_1 = Stage2_1_type {              lv_quotient     : lv_quotient,
                                                    lv_remainder    : lv_remainder,
                                                    exponent        : lv_exponent,
                                     				sign            : lv_sign,
                                     				infinity        : lv_infinity,
                                     				invalid         : lv_invalid,
                                     				dz              : lv_dz,
                                     				zero            : lv_zero,
                                     				rounding_mode   : lv_rounding_mode,
                                                    quiet_nan       : quiet_nan
                                 };
        ff_stage2_1.enq( stage2_1);

    endrule
    
	rule rl_stage3;//(rg_state_handler == Stage2 && !wr_flush);
        

        let stage2_1 = ff_stage2_1.first ; ff_stage2_1.deq;
        Bit#(27) lv_quotient        = stage2_1.lv_quotient;
        Bit#(28) lv_remainder       = stage2_1.lv_remainder;
        Bit#(10) lv_exponent 	    = stage2_1.exponent;
		Bit#(1)  lv_sign			= stage2_1.sign;
		Bit#(1)  lv_infinity    	= stage2_1.infinity;
		Bit#(1)  lv_invalid 		= stage2_1.invalid;
		Bit#(1)  lv_dz 				= stage2_1.dz;
		Bit#(1)  lv_zero 			= stage2_1.zero;
		Bit#(3)  lv_rounding_mode	= stage2_1.rounding_mode;
        let quiet_nan = stage2_1.quiet_nan;

        Bit#(TSub#(8,1)) bias = '1;
		bit lv_underflow = 0;
		bit lv_overflow = 0;

		Int#(10) lv_actual_exponent = unpack(lv_exponent - {3'b0,bias});
        //Change-1 Removing not_required variable
       // Int#(23) lv_actual_exponent_temp = signExtend(lv_actual_exponent); 
		let msb_zeros = pack(countZerosMSB(lv_quotient));
    `ifdef verbose $display("MSB Zeros: %d",msb_zeros); `endif
		let lsb_zeros = 0;

		// lv_quotient_is_subnormal construct is like a flag which can be used in difficult situations
		bit lv_quotient_is_subnormal = 0;
		bit lv_sticky = lv_quotient[0];
        //Bit#(23) bias_temp = zeroExtend(bias);
		/*
		if exponent is > 128 then obviously none of the numbers are subnormal
		so the product is of the form 1x.xxxx or 01.xxxx
		the overflow conditions are handled in the following if condition accordingly
		*/
		if(lv_actual_exponent > unpack({3'b0,bias} + 1)) begin //CHECK THIS CASE WITHOUT FAIL - OPTIMIZE IT
			lv_overflow = 1;
			`ifdef verbose $display("lv_overflow!!!"); `endif
		end
		/*     
                -bias -fPMAN 
		-150  = -126 -23 -1
        -1075 = -1022 -52 -1 //for DP?
		-1 is for the implicit bit
		i.e. if all the bits are shifted out then its an underflow
		*/
		else if(lv_actual_exponent < unpack(-zeroExtend(bias)-fromInteger(fPMAN)-1)) begin            //TODO What here? TODO Check <-150 or <-151
		//else if(lv_actual_exponent_temp < unpack(-bias_temp-fromInteger(fPMAN)-1)) begin            //TODO What here? TODO Check <-150 or <-151
            //`ifdef verbose $display("lv_actual_exponent : %d bias-23-1 :  %d", lv_actual_exponent, -bias_temp-fromInteger(fPMAN-1)); `endif
			lv_underflow = 1;
			lv_quotient = 1;
			lv_exponent = 0;
			//When the exponent is < -151, sticky bit is automatically set to one
			`ifdef verbose $display("lv_underflow!!!"); `endif
		end
		 	
		else begin

			// possible shift is positive when exponent is lesser than -126
            //Change-x it's enough if possible shift is reduced from lv_exponent - reducing again from bias is actually redundant and incurs another adder
            //Same Experiment here, do all the if-else parallely and just use the if and else for assignments
			Int#(10) possible_shift = 1-unpack(lv_exponent);
			`ifdef verbose $display("possible_shift = %0d", possible_shift); `endif
			
            lsb_zeros = pack(countZerosLSB(lv_quotient));
			
            let lv_quotient_shiftR = lv_quotient >> pack(possible_shift);
			//lv_quotient = {lv_quotient[fPMAN+3:1], lv_quotient[0] | lv_sticky};
            let lv_exponent_inc_shift = lv_exponent + pack(possible_shift);
            let shift_neg = (~pack(possible_shift)+1);
            let lv_quotient_shiftL_expo = lv_quotient << shift_neg;
			let lv_exponent_sub_shift = lv_exponent - shift_neg;
            let lv_quotient_shiftL_zerosMSB = lv_quotient << (msb_zeros);
			let lv_exponent_sub_zerosMSB = lv_exponent - (zeroExtend(msb_zeros));



			if(possible_shift > 0) begin

				//Setting sticky if all lsb zeros are removed out

				if(possible_shift > unpack(zeroExtend(lsb_zeros)) || lv_quotient[0] == 1) 
                    lv_sticky = 1;

				//Handling sticky
					//lv_sticky = lv_quotient[0];
                lv_quotient = {lv_quotient_shiftR[fPMAN+3:1],lv_quotient_shiftR[0]|lv_sticky};
                lv_sticky   = lv_quotient[0];
                lv_exponent = lv_exponent_inc_shift;

				`ifdef verbose $display("lv_quotient = %h since exp < -126", lv_quotient); `endif
				`ifdef verbose $display("and thus the sticky bit = %b", lv_sticky); `endif

              `ifdef verbose  $display("lv_exponent : %b",lv_exponent); `endif
				lv_quotient_is_subnormal = 1;
			end

			/*
			msb_zeros != 1 means product is of the form 00.xxxx, important case
			*/
			else if(msb_zeros != 0) begin
				/*
				if possible shift is < the number of leading zeros then the number can't be made normal
				*/
				if(shift_neg < zeroExtend(msb_zeros)) begin
				    lv_quotient = lv_quotient_shiftL_expo;
                    lv_exponent = lv_exponent_sub_shift;
					lv_quotient_is_subnormal = 1;
				end
				/*
				if exponent affords to give away enough such that shifting left leads to 01.xxxx and exponent >= -126
				*/
				else begin
					lv_quotient = lv_quotient_shiftL_zerosMSB;
					lv_exponent = lv_exponent_sub_zerosMSB;
					lv_quotient_is_subnormal = 0;
	 			end
			end
		end

        if(lv_quotient_is_subnormal == 1)
            lv_exponent = 0;

        //rg_state_handler <= Stage3;
        let stage3 = Stage3_type{ 
                                    lv_quotient      : lv_quotient,
                                    lv_remainder     : lv_remainder,
                                    lv_exponent      : lv_exponent,
                                    lv_sign          : lv_sign,
                                    lv_infinity      : lv_infinity,
                                    lv_invalid       : lv_invalid,
                                    lv_underflow     : lv_underflow,
                                    lv_overflow      : lv_overflow,
                                    lv_dz            : lv_dz,
                                    lv_zero          : lv_zero,
                                    lv_sticky        : lv_sticky,
                                    lv_quotient_is_subnormal : lv_quotient_is_subnormal,
                                    lv_rounding_mode : lv_rounding_mode,
                                    quiet_nan        : quiet_nan
                                };
        ff_stage3.enq(stage3);

        endrule
        //------------------------------------------------------------------Ex-1 Splitting Here ---------------------------------------------------------//
       //Required - 1. lv_quotient 2. lv_remainder 3. lv_quotient_is_subnormal 4. lv_rounding_mode 5. all exception lv's
        //Splitting the stage here
	      
		rule rl_stage4;//(rg_state_handler==Stage3 && !wr_flush);
        let stage3 = ff_stage3.first; ff_stage3.deq ;

        let lv_quotient  = stage3.lv_quotient;
        let lv_remainder = stage3.lv_remainder;
        let lv_exponent  = stage3.lv_exponent;
        let lv_sign      = stage3.lv_sign;
        let lv_infinity  = stage3.lv_infinity;
        let lv_invalid   = stage3.lv_invalid;
        let lv_dz        = stage3.lv_dz;
        let lv_zero      = stage3.lv_zero;
        let lv_rounding_mode = stage3.lv_rounding_mode;
        let lv_overflow = stage3.lv_overflow;
        let lv_underflow = stage3.lv_underflow;
        let lv_sticky = stage3.lv_sticky;
        let lv_quotient_is_subnormal = stage3.lv_quotient_is_subnormal;
        let quiet_nan = stage3.quiet_nan;
       
        `ifdef verbose $display("lv_quotient = %h, lv_remainder = %h, lv_exponent = %h", lv_quotient, lv_remainder, lv_exponent); `endif

		bit lv_guard = lv_quotient[2];  
		bit lv_round = lv_quotient[1];			
 		bit lv_inexact = 0;				
		bit lv_round_up = 0;						
           
		if(lv_remainder!=0 || lv_quotient[0] == 1) // if the remainder is zero, sticky bit is set to 1.
			lv_sticky = 1;

		if((lv_sticky | lv_guard | lv_round) == 1)// if any of the sticky,guard or round bit is set, the value is inexact.
			lv_inexact = 1;

        if(lv_inexact == 1 && lv_quotient_is_subnormal == 1) //Was buried deep inside the SPEC. Phew! Maybe Wrong!!!
            lv_underflow = 1;

		// Following if-else condition determine the value of lv_round_up. If set, the mantissa needs to be incremented, else the mantissa remains unchanged.
		if(lv_rounding_mode == 'b000) 
			lv_round_up = lv_guard & (lv_round|lv_sticky|lv_quotient[3]);
		else if(lv_rounding_mode == 'b100)
			lv_round_up = lv_guard; //& (lv_round|lv_sticky|lv_sign);
		else if(lv_rounding_mode == 'b011) 
			lv_round_up = (lv_guard|lv_round|lv_sticky) & ~lv_sign;
		else if(lv_rounding_mode == 'b010)
            lv_round_up = (lv_guard|lv_round|lv_sticky) & lv_sign;

        // otherwise if round to zero mode, then do nothing

		Bit#(25) lv_rounded_quotient = {1'b0,lv_quotient[fPMAN+3:3]};

		if( lv_round_up == 1) begin
			lv_rounded_quotient = lv_rounded_quotient + 1;
		end

		if(lv_rounded_quotient[fPMAN+1] == 1 ) begin
      `ifdef verbose $display("Exponent Incremented 1"); `endif
			lv_exponent = lv_exponent + 1;
			lv_rounded_quotient = lv_rounded_quotient >> 1;
		end
		if(lv_quotient[fPMAN+3] == 0 && lv_rounded_quotient[fPMAN] == 1) begin
      `ifdef verbose $display("Exponent Incremented 2"); `endif
			lv_exponent = lv_exponent + 1;
        end
        
        /*let stage4 = Stage4_type{ 
                lv_rounded_quotient     : lv_rounded_quotient,
                lv_exponent             : lv_exponent,
                lv_sign                 : lv_sign,
                lv_infinity             : lv_infinity,
                lv_invalid              : lv_invalid,
                lv_dz                   : lv_dz,
                lv_underflow            : lv_underflow,
                lv_overflow             : lv_overflow,
                lv_zero                 : lv_zero,
                lv_rounding_mode        : lv_rounding_mode,
                quiet_nan               : quiet_nan,
                lv_inexact              : lv_inexact 
            };
        ff_stage4.enq(stage4);

    endrule

    rule rl_stage5;
    
    let stage4 = ff_stage4.first ; ff_stage4.deq ;
    
    let lv_rounded_quotient     = stage4.lv_rounded_quotient;
    let lv_exponent             = stage4.lv_exponent;
    let lv_sign                 = stage4.lv_sign;
    let lv_infinity             = stage4.lv_infinity;
    let lv_invalid              = stage4.lv_invalid;
    let lv_dz                   = stage4.lv_dz;
    let lv_underflow            = stage4.lv_underflow;
    let lv_overflow             = stage4.lv_overflow;
    let lv_zero                 = stage4.lv_zero;
    let lv_rounding_mode        = stage4.lv_rounding_mode;
    let quiet_nan               = stage4.quiet_nan;
    let lv_inexact              = stage4.lv_inexact; */
       
    Bit#(8) out_exp = lv_exponent[fPEXP-1:0];
    Bit#(23) out_man = lv_rounded_quotient[fPMAN-1:0];
    Bit#(8) exp_all_zeros = '0;
	Bit#(8) exp_all_ones = '1;
    Bit#(TSub#(8,1)) exp_all_ones_1 = '1;
    Bit#(23) man_all_zeros = '0;
    Bit#(23) man_all_ones = '1;
    Bit#(TSub#(23,1)) man1_all_zeros = '0;
    Bit#(TSub#(23,1)) man_all_ones_1 = '1;
	Bit#(32) lv_final_output= 0;
	Bit#(5) exception = 0;  
     
		// result is infinity
        if(lv_infinity == 1) begin              
			lv_final_output = {lv_sign, exp_all_ones, man_all_zeros};
            if(lv_dz==1)
                exception[3] = 1;
        end

		// the result is invalid
		else if(lv_invalid == 1) begin              
      lv_final_output = {1'b0, exp_all_ones,1'b1, man1_all_zeros};
    	exception[4] = ~quiet_nan;//Invalid;
		end
	// operation is divide by zero
    else if(lv_dz==1) begin
      lv_final_output= {lv_sign, exp_all_ones, man_all_zeros};
      exception[3] = 1;//Divide_by_Zero;
    end
		// result is zero
    else if(lv_zero == 1)                  
      lv_final_output={lv_sign,exp_all_zeros,man_all_zeros};
        // result is underflow
    else if(lv_underflow == 1) begin
       
      lv_final_output= {lv_sign,exp_all_zeros,lv_rounded_quotient[fPMAN-1:0]};       	//TODO to verify if it needs to be lv_rounded_quotient[22:1] and lv_inexact bit.
    	exception[1] = 1;//Underflow;
        exception[0] = 1;
    end
        // result is overflow
    else if(lv_overflow == 1 || out_exp == '1) begin
      exception[2] = 1;//Overflow;
      exception[0] = 1; //inexact -- is it? let's see!
      if(lv_rounding_mode == 'b001)
				lv_final_output = {lv_sign,{exp_all_ones_1,1'b0},man_all_ones};
			else if(lv_rounding_mode == 'b010 && lv_sign ==0)
				lv_final_output = {lv_sign,{exp_all_ones_1,1'b0},man_all_ones};
			else if(lv_rounding_mode == 'b011 && lv_sign==1)
				lv_final_output = {lv_sign,{exp_all_ones_1,1'b0},man_all_ones};
			else 
				lv_final_output ={lv_sign,exp_all_ones,man_all_zeros};
    end
    else begin
      lv_final_output = {lv_sign,out_exp,out_man};
			if(lv_inexact==1)
    			exception[0] = 1;//Inexact;
		end
        //rg_state_handler <= Begin;
    // Forming the new Floating point Status Register
        // Enqueing the final result into the output FIFO
    wr_final_out <= Floating_output{ 
    		                     	  final_result    : lv_final_output,//Appending zeros at the MSB since the result is a Single Precision number which is 32-bits wide whereas the rob entries are 64-bits.
    		                     	  fflags       : exception};

	endrule

	method Action _start(Bit#(1) lv_sign, Bit#(23) lv_mantissa1, Bit#(8) lv_exponent1, Bit#(23) lv_mantissa2, Bit#(8) lv_exponent2, Bit#(3) rounding_mode, Tuple2#(Bit#(5),Bit#(5)) flags);
 
        Bit#(TSub#(8,1)) bias = '1;
        let condFlags1 = tpl_1(flags);
        let condFlags2 = tpl_2(flags);
        Int#(8) actual_exponent1 = unpack(lv_exponent1 - {1'b0,bias});
		Int#(8) actual_exponent2 = unpack(lv_exponent2 - {1'b0,bias});
        `ifdef verbose $display("Exp1: %h, Man1: %h, Exp2: %h Man2: %h",lv_exponent1,lv_mantissa1,lv_exponent2,lv_mantissa2); `endif
        `ifdef verbose $display("condFlags1 : %b condFlags2: %b",condFlags1,condFlags2); `endif
		Bit#(1) lv_inf = 0;
		Bit#(1) lv_inv = 0;
		Bit#(1) lv_zero = 0;
		Bit#(1) lv_dz = 0;
		bit quiet_nan = condFlags1[2] & ~condFlags2[0]  | condFlags2[2] & ~condFlags1[0] ;
		Bit#(1) lv_op1_is_zero = condFlags1[3];			                    //1 when operand1=0
		Bit#(1) lv_op2_is_zero = condFlags2[3];                 			//1 when operand2=0

		Bit#(1) lv_op1_subnormal = condFlags1[4];		                    //1 when operand1 is subnormal
		Bit#(1) lv_op2_subnormal = condFlags2[4];                   		//1 when operand2 is subnormal

		Bit#(1) lv_op1_is_infinity = condFlags1[1];
		Bit#(1) lv_op2_is_infinity = condFlags2[1];

		`ifdef verbose $display("op1 is subnormal = %b , op2 is subnormal = %b", lv_op1_subnormal, lv_op2_subnormal); `endif
	//	`ifdef verbose $display("sign1 = %b exponent1 = %b actual_exponent1 = %0d mantissa1 = %b.%b", _operand1[31], _operand1[fPINP-2:fPMAN], actual_exponent1, ~lv_op1_subnormal, _operand1[fPMAN-1:0]); `endif
//		`ifdef verbose $display("sign2 = %b exponent2 = %b actual_exponent2 = %0d mantissa2 = %b.%b", _operand2[31], _operand2[fPEXP-1:fPMAN], actual_exponent2, ~lv_op2_subnormal, _operand2[fPMAN-1:0]); `endif


	  if(((condFlags1[2] | condFlags1[0])==1) || ((condFlags2[2] | condFlags2[0])==1) || (lv_op1_is_infinity == 1 && lv_op2_is_infinity == 1) || (lv_op1_is_zero == 1 && lv_op2_is_zero == 1)) begin  	//op1 or op2 are NaN (or) both are infinity (or) both are zero
	  	lv_inv = 1;                           					//result is invalid
		end
		else if(lv_op1_is_infinity ==1) begin       			//op 2 is neither NaN nor infinity, and op1 is infinity
	    lv_inf=1;                          						//result is infinity
		end
	  else if(lv_op2_is_zero==1) begin            				//op 1 is neither NaN nor infinity, and op2 is zero
      lv_inf=1;                          						//result is infinity
     	lv_dz=1;                                				//setting the divide by zero flag
    end
    else if(lv_op2_is_infinity == 1 || lv_op1_is_zero == 1)   	//{op1 and op2 are not NaN} (and) {op1 is zero and op2 is not zero (or) op2 is infinity and op1 is not infinity}
      lv_zero=1;                                  				//result is zero


    let man1 ={~lv_op1_subnormal,lv_mantissa1};
    let man2 ={~lv_op2_subnormal,lv_mantissa2};
    let zeros1 =countZerosMSB(man1);
    let zeros2 =countZerosMSB(man2);
    man1=man1<<zeros1;
    man2=man2<<zeros2;
    Bit#(10) exp1={2'b0,lv_exponent1};
    Bit#(10) exp2={2'b0,lv_exponent2};
    exp1=exp1- zeroExtend(unpack(pack(zeros1)));
    exp2=exp2- zeroExtend(unpack(pack(zeros2)));
        /*
        total_baised_exponent = (biased_exponent - bias) - (1 - bias) + bias 					in the case of normal divided by subnormal
        total_biased_exponent = (1 - bias) - (biased_exponent - bias) + bias 					in the case of subnormal divided by normal
        total_biased_exponent = (biased_exponent - bias) - (biased_exponent - bias) + bias 		in the case of normal divided by normal
        total_biased_exponent = (1 - bias) - (1 - bias) + bias 									in the case of subnormal divided by normal

		SO equivalently to handle all the cases:
		total_biased_exponent = bias + (op1_biased_expo + is_op1_denorm) - (op2_biased_expo + is_op2_denorm)
        */

    Bit#(10) lv_exponent = {3'b0,bias} + ((exp1 + zeroExtend(lv_op1_subnormal)) - (exp2 + zeroExtend(lv_op2_subnormal))); //error will come obv


		Int#(10) lv_actual_exponent = unpack(lv_exponent - {3'b0,bias});
		`ifdef verbose $display("lv_sign: %h lv_exponent = %h, lv_actual_exponent = %d",lv_sign, lv_exponent, lv_actual_exponent); `endif
    `ifdef verbose $display("lv_inv: %b lv_inf %b lv_dz %b lv_zero %b",lv_inv,lv_inf,lv_dz,lv_zero); `endif
     // rg_state_handler <= Stage1; 
	  let stage1 = Stage1_type  {	    exponent		: lv_exponent,
    	  			               			           	dividend		: man1,
				       	                    			divisor			: man2,
                                                     	sign        	: lv_sign,
        				        	            		invalid			: lv_inv,
        				        	            		infinity		: lv_inf,
        				        		            	dz	    		: lv_dz,
        				        		            	zero			: lv_zero,
										            	rounding_mode 	: rounding_mode,
                                  quiet_nan  : quiet_nan
                                                 };
        ff_stage1.enq(stage1);
        
	endmethod

    // Output method which send the result
	method Floating_output#(32) final_result_();
	    return wr_final_out;
	endmethod

    method Action flush;
        wr_flush <= True;
    endmethod
	
    // This method needs to be called whenever the method final_result is called.
    // This method frees the final FIFO and resets the ready signal.

endmodule

		/*		TEST BENCH 		*/

//(*synthesize*)
/*
module mkTb_fpu_divider_pipe_32(Empty);
	
    function Tuple2#(Bit#(5), Bit#(5)) condFlags (Tuple2#(Bit#(52), Bit#(11)) x, Tuple2#(Bit#(52), Bit#(11)) y);
        let man1  = tpl_1(x);
        let expo1 = tpl_2(x);
        let man2  = tpl_1(y);
        let expo2 = tpl_2(y);
        Bit#(5) flags1, flags2;
        Bool expZ1 = (expo1 == 0);
        Bool manZ1 = (man1  == 0);
        Bool expO1 = (expo1 == '1);
        Bool manO1 = (man1  == '1);
        Bool topB1 = (man1[51] == 1);
        Bool expZ2 = (expo2 == 0);
        Bool manZ2 = (man2  == 0);
        Bool expO2 = (expo2 == '1);
        Bool manO2 = (man2  == '1);
        Bool topB2 = (man2[51] == 1);
        flags1 = {pack(expZ1 && !manZ1),pack(manZ1 && expZ1),pack(expO1 && topB1),pack(expO1 && manZ1),pack(expO1 && !topB1)}; //Denormal, isZero, QNaN, Infinity, SNaN
        flags2 = {pack(expZ2 && !manZ2),pack(manZ2 && expZ2),pack(expO2 && topB2),pack(expO2 && manZ2),pack(expO2 && !topB2)}; //Denormal, isZero, QNaN, Infinity, SNaN
        return tuple2(flags1,flags2);
    endfunction

    function Tuple2#(Bit#(52),Bit#(52)) getMantissa (Bit#(64) op1, Bit#(64) op2);
        return tuple2(op1[51:0],op2[51:0]);
    endfunction

    function Tuple2#(Bit#(11), Bit#(11)) getExp (Bit#(64) op1, Bit#(64) op2);
        return tuple2(op1[62:52], op2[62:52]);
    endfunction


//    Reg#(Bit#(64)) rg_operand1<-mkReg(64'h006069e454c8dc70); 
//	Reg#(Bit#(64)) rg_operand2<-mkReg(64'h41e4864ef5800000);
    Reg#(Bit#(64)) rg_operand1<-mkReg(64'h00000000fffff89d); 
	Reg#(Bit#(64)) rg_operand2<-mkReg(64'hbff0000000000000);

	Reg#(Bit#(32)) rg_clock<-mkReg(0); 
	Ifc_fpu_divider_pipe_32#(64,52,11) divider<-mkfpu_divider_pipe_32();

	Reg#(Bit#(32)) rg_arbit <-mkReg(0);

	rule rl_clk_count;
		rg_clock<=rg_clock+1;
	endrule


	rule rl_start_1(rg_clock=='d0);
            let {man1,man2} =  getMantissa(rg_operand1, rg_operand2);
            let {exp1,exp2} =  getExp(rg_operand1, rg_operand2);
            let {x1,x2}           =  condFlags(tuple2(man1,exp1),tuple2(man2,exp2));
		 `ifdef verbose $display("Giving inputs rg_operand 1 : %h rg_operand 2 : %h through testbench",rg_operand1,rg_operand2,$time); `endif
		divider._start(rg_operand1[63]^rg_operand2[63],man1,exp1,man2,exp2,3'b100,tuple2(x1,x2));
	endrule

	rule rl_display_result;
         let abc = divider.final_result_();
         `ifdef verbose $display("output: %h fflags: %h",abc.final_result, abc.fflags); `endif
	endrule

	rule rl_finish_(rg_clock=='d60);
		$finish(0);
	endrule

endmodule:mkTb_fpu_divider_pipe_32
*/
//module mkTb_fpu_divider_pipe_32_2(Empty);
//	
//	RegFile #(Bit #(10), Bit #(68))  input_data <- mkRegFileFullLoad("./testcases/Div_denormal_testcases.hex");
//	Reg #(Bit #(10)) index <- mkReg(0);
//	
//	Reg #(Bit #(32)) state_clock <- mkReg(1);
//    Reg #(Bit #(32)) rg_state <- mkReg(0);
// 	/*****************Module Instantiation******************************/
//	Ifc_fpu_divider_pipe_32#(32,23,8) divider <- mkfpu_divider_pipe_32();
//
//
//	/******************File Creation************************************/
//	Reg#(int) cnt <- mkReg(0);                  //File Creation counter
//	let fh <- mkReg(InvalidFile) ;				//File Handler
//	rule open (cnt == 0 ) ;
//		File tb_mul_output <- $fopen("tb_div_output.hex", "w+"); 
//		fh <= tb_mul_output;
//		cnt <= 1 ;
//	endrule
//
//	/******************clock_count**************************************/
//	rule state_clock_count;
//		state_clock <= state_clock + 1;
//	endrule
//
//	/*******************input******************************************/
//	rule take_input_in (rg_state == 0);
//		divider._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0]);
//		index <= index + 1;
//        rg_state <= 1;
//	endrule
//
//	/*******************output*****************************************/
//	rule display_output (rg_state == 1);
//        let abc = divider.final_result_();
//		$fwrite(fh, "%h\n", abc.final_result[31:0]);
//        rg_state <= 0;
//	endrule
//
//	/******************end testing*************************************/
//	rule end_testing (index == 407);
//		$finish(0);
//	endrule
//
//endmodule
endpackage
