/***************************************************
----------------------top mod-----------------------
***************************************************/
module prj2(
				clk,
				rst,
				MemRW_IO,
				MemAddr_IO,
				MemD_IO
			);		
input clk;
input rst;
output MemRW_IO;
output [7:0]MemAddr_IO;
output [15:0]MemD_IO;

wire zflag, muxPC, muxMAR, muxACC, loadMAR, loadPC, loadACC, loadMDR, loadIR, MemRW;
wire [2:0] opALU;
wire [7:0] opcode, MemAddr;
wire [15:0] MemD, MemQ;
wire load_div, rst_div, done_dive;
wire load_exp, done_exp; //proj2

ram ram_ins(
				.we(MemRW),
				.d(MemD), 
				.q(MemQ), 
				.addr(MemAddr)
				);

datapath datapathproj(
				.clk(clk),
				.rst(rst),
				.muxPC(muxPC),
				.muxMAR(muxMAR),
				.muxACC(muxACC),
				.loadMAR(loadMAR),
				.loadPC(loadPC),
				.loadACC(loadACC),
				.loadMDR(loadMDR),
				.loadIR(loadIR),
				.opALU(opALU),
				.zflag(zflag),
				.opcode(opcode),
				.MemAddr(MemAddr),
				.MemD(MemD),
				.MemQ(MemQ),
				.Done_s(done_dive),
				.Load_s(load_div),
				.Done_e(done_exp),
				.Load_e(load_exp)//proj2
				);			
				
ctr ctrproj(
				.clk(clk),
				.rst(rst),
				.zflag(zflag),
				.opcode(opcode),
				.muxPC(muxPC),
				.muxMAR(muxMAR),
				.muxACC(muxACC),
				.loadMAR(loadMAR),
				.loadPC(loadPC),
				.loadACC(loadACC),
				.loadMDR(loadMDR),
				.loadIR(loadIR),
				.opALU(opALU),
				.MemRW(MemRW),
				.load_div(load_div),
				.rst_div(rst_div), //divder load and rst and done
				.done_div (done_dive),
				.load_exp(load_exp),
				.done_exp(done_exp)//proj2
				);
assign MemAddr_IO = MemAddr;
assign MemD_IO = MemD;
assign MemRW_IO = MemRW;
endmodule


/***************************************************
--------------------alu_sub_mod---------------------
***************************************************/
module my1bitmux(
		output out, // header & ports
		input i0, i1, sel
		);
		
wire n_sel, x1, x2; // internal nets
or (out, x1, x2); // output
and (x1, i0, n_sel); // i0 & (~sel)
and (x2, i1, sel); // i1 & sel
not (n_sel, sel); // invert sel
endmodule


// And Gate using 2-1 mux
module muxand(output y, input a, input b);
  supply0 gnd;
  supply1 pwr;
  my1bitmux mux1(y,gnd,a,b);
endmodule


// Xor Gate using 2-1 mux
module muxxor(output y, input a, input b);
  supply0 gnd;
  supply1 pwr;
  wire between;
  my1bitmux mux2(between,pwr,gnd,b);
  my1bitmux mux3(y,b,between,a);
endmodule

module my16bitsmuxxor(output[15:0] y, input[15:0] a, input[15:0] b); 
   muxxor xor11(y[0], a[0], b[0]);
   muxxor xor22(y[1], a[1], b[1]);
   muxxor xor33(y[2], a[2], b[2]);
   muxxor xor44(y[3], a[3], b[3]);
   muxxor xor55(y[4], a[4], b[4]);
   muxxor xor66(y[5], a[5], b[5]);
   muxxor xor77(y[6], a[6], b[6]);
   muxxor xor88(y[7], a[7], b[7]);
   muxxor xor99(y[8], a[8], b[8]);
   muxxor xor1010(y[9], a[9], b[9]);
   muxxor xor1111(y[10], a[10], b[10]);
   muxxor xor1212(y[11], a[11], b[11]);
   muxxor xor1313(y[12], a[12], b[12]);
   muxxor xor1414(y[13], a[13], b[13]);
   muxxor xor1515(y[14], a[14], b[14]);
   muxxor xor1616(y[15], a[15], b[15]);
endmodule
  

// Or Gate using 2-1 mux
module muxor(output y, input a, input b);
  supply0 gnd;
  supply1 pwr;
  my1bitmux mux4(y,b,pwr,a);
endmodule


// Not gate using 2-1 mux
module muxnot(output y, input a);
  supply0 gnd;
  supply1 pwr;
  my1bitmux mux5(y,pwr,gnd,a);
endmodule

module my16bitmuxnot(output [15:0]y, input [15:0] a);
  muxnot mn0(y[0], a[0]);
  muxnot mn1(y[1], a[1]);
  muxnot mn2(y[2], a[2]);
  muxnot mn3(y[3], a[3]);
  muxnot mn4(y[4], a[4]);
  muxnot mn5(y[5], a[5]);
  muxnot mn6(y[6], a[6]);
  muxnot mn7(y[7], a[7]);
  muxnot mn8(y[8], a[8]);
  muxnot mn9(y[9], a[9]);
  muxnot mn10(y[10], a[10]);
  muxnot mn11(y[11], a[11]);
  muxnot mn12(y[12], a[12]);
  muxnot mn13(y[13], a[13]);
  muxnot mn14(y[14], a[14]);
  muxnot mn15(y[15], a[15]);
endmodule

// Half adder using Xor and And modules
module my1bithalfadder(output sum, carry, input A, B);
  muxxor o1(sum,A,B);
  muxand a1(carry,A,B);
endmodule


// Full adder from half adder
module my1bitfulladder(output Cout, S, input A,B,Cin);
  wire x1,x2,x3;
  my1bithalfadder m0(x2,x1,A,B);
  my1bithalfadder m1(S,x3,x2,Cin);
  muxor O1(Cout,x1,x3);
endmodule


//4bit full addder from 1 bit full adder
module my4bitfulladder(output [3:0] S, output Cout, input [3:0] A, B, input Cin);
  wire [2:0] c;
  my1bitfulladder a0(c[0], S[0], A[0],B[0],Cin);
  my1bitfulladder a1(c[1], S[1], A[1],B[1],c[0]);
  my1bitfulladder a2(c[2], S[2], A[2],B[2],c[1]);
  my1bitfulladder a3(Cout, S[3], A[3],B[3],c[2]);
endmodule

//4bit multiplexer
module my4bitmux(output [3:0] Out,
 input [3:0] A, B, input sel);
 my1bitmux m3 (Out[3], A[3], B[3], sel);
 my1bitmux m2 (Out[2], A[2], B[2], sel);
 my1bitmux m1 (Out[1], A[1], B[1], sel);
 my1bitmux m0 (Out[0], A[0], B[0], sel);
endmodule

//add and sub from 4bit full adder
module my4bitaddsub_gate(output [3:0] O, output Cout, input [3:0] A, B, input S);  
  supply0 gnd;
  wire [3:0] b_n,b_carry;
  muxnot n0(b_n[0], B[0]);
  muxnot n1(b_n[1], B[1]);
  muxnot n2(b_n[2], B[2]);
  muxnot n3(b_n[3], B[3]);
  my4bitmux m0(b_carry,B,b_n,S);
  my4bitfulladder fa0(O,Cout,A,b_carry,S);
endmodule

module my16bitfulladder(output [15:0] S, output Cout, input [15:0] A, B, input Cin);
  wire [14:0] c;
  my1bitfulladder my0(c[0], S[0], A[0],B[0],Cin);
  my1bitfulladder my1(c[1], S[1], A[1],B[1],c[0]);
  my1bitfulladder my2(c[2], S[2], A[2],B[2],c[1]);
  my1bitfulladder my3(c[3], S[3], A[3],B[3],c[2]);
  my1bitfulladder my4(c[4], S[4], A[4],B[4],c[3]);
  my1bitfulladder my5(c[5], S[5], A[5],B[5],c[4]);
  my1bitfulladder my6(c[6], S[6], A[6],B[6],c[5]);
  my1bitfulladder my7(c[7], S[7], A[7],B[7],c[6]);
  my1bitfulladder my8(c[8], S[8], A[8],B[8],c[7]);
  my1bitfulladder my9(c[9], S[9], A[9],B[9],c[8]);
  my1bitfulladder my10(c[10], S[10], A[10],B[10],c[9]);
  my1bitfulladder my11(c[11], S[11], A[11],B[11],c[10]);
  my1bitfulladder my12(c[12], S[12], A[12],B[12],c[11]);
  my1bitfulladder my13(c[13], S[13], A[13],B[13],c[12]);
  my1bitfulladder my14(c[14], S[14], A[14],B[14],c[13]);
  my1bitfulladder my15(Cout, S[15], A[15],B[15],c[14]);   
endmodule

module my16bitmux(output [15:0] Out,
  input [15:0] A, B, input sel);
  my1bitmux mb15 (Out[15], A[15], B[15], sel);
  my1bitmux mb14 (Out[14], A[14], B[14], sel);
  my1bitmux mb13 (Out[13], A[13], B[13], sel);
  my1bitmux mb12 (Out[12], A[12], B[12], sel);
  my1bitmux mb11 (Out[11], A[11], B[11], sel);
  my1bitmux mb10 (Out[10], A[10], B[10], sel);
  my1bitmux mb9 (Out[9], A[9], B[9], sel);
  my1bitmux mb8 (Out[8], A[8], B[8], sel);
  my1bitmux mb7 (Out[7], A[7], B[7], sel);
  my1bitmux mb6 (Out[6], A[6], B[6], sel);
  my1bitmux mb5 (Out[5], A[5], B[5], sel);
  my1bitmux mb4 (Out[4], A[4], B[4], sel);
  my1bitmux mb3 (Out[3], A[3], B[3], sel);
  my1bitmux mb2 (Out[2], A[2], B[2], sel);
  my1bitmux mb1 (Out[1], A[1], B[1], sel);
  my1bitmux mb0 (Out[0], A[0], B[0], sel);
endmodule
//plus and minus
module my16bitaddsub_gate(output [15:0] O, output Cout, input [15:0] A, B, input S);
  supply0 gnd;
  wire [15:0] b_n,b_carry;
  muxnot nb0(b_n[0], B[0]);
  muxnot nb1(b_n[1], B[1]);
  muxnot nb2(b_n[2], B[2]);
  muxnot nb3(b_n[3], B[3]);
  muxnot nb4(b_n[4], B[4]); 
  muxnot nb5(b_n[5], B[5]); 
  muxnot nb6(b_n[6], B[6]); 
  muxnot nb7(b_n[7], B[7]);
  muxnot nb8(b_n[8], B[8]);
  muxnot nb9(b_n[9], B[9]);
  muxnot nb10(b_n[10], B[10]);
  muxnot nb11(b_n[11], B[11]);
  muxnot nb12(b_n[12], B[12]);
  muxnot nb13(b_n[13], B[13]);
  muxnot nb14(b_n[14], B[14]);
  muxnot nb15(b_n[15], B[15]);  
  my16bitmux mm(b_carry,B,b_n,S);
  my16bitfulladder mf(O,Cout,A,b_carry,S);
endmodule
 
//divider
module my16bitdivider(output reg [15:0] Q,R, output reg Done, input [15:0] A, B, input Load,Clk, input Reset);
 reg [15:0] A_temp, B_temp, A_reg, B_reg;
 reg [2:0] current_state, next_state;
 reg S_temp, C_out; //Done wire or reg?
 wire [15:0] O_temp;
 wire C_temp;
 my16bitaddsub_gate my88(O_temp, C_temp, A_temp, B_temp, S_temp);
 parameter S000 = 3'b000, S001 = 3'b001, S010 = 3'b010, S011 = 3'b011, S100 = 3'b100, S101 = 3'b101;
 
 always @(posedge Clk) begin 
 if (Reset) current_state <= S000;
 else current_state <= next_state;
end  

 always @(*) begin
  case (current_state)
  S000: if(Load) next_state = S001; 
        else next_state = S000; 
  S001: if (R>=B_reg) next_state = S010;  
 	   else if (R<B_reg) next_state = S101;                            
  S010: next_state = S011; 
  S011: next_state = S100; 
  S100: next_state = S001; 
  S101: next_state = S000; 
  default: next_state = S000;
  endcase
end
  
always @(posedge Clk) begin
 if (Reset) begin
  A_reg = 0;
  B_reg = 0;
 end
 else if (current_state == S000 && Load == 1)begin
  A_reg <= A;
  B_reg <= B;
 end
end
 
always @(posedge Clk) begin
if (Reset) begin	
 R <= 0;
end
else if (current_state == S000 && Load == 1)begin
 R <= A;
end
else if (current_state == S010)begin
 R <= O_temp; //In state 3, we use plus function of 8bitaddsubgate, so we need copy O_temp to R
end
end

always @(posedge Clk) begin
if (Reset) begin	
 Q <= 1'd0;
end  
else if (current_state == S011)begin
 Q <= O_temp; //In state 4. we need save value
end
end

always @(posedge Clk) begin
if (current_state == S011)begin
 C_out <= C_temp; //?
end
else if (current_state == S011)begin
 C_out <= C_temp;  //?
end
end

always @(*) begin
if (current_state == S010)begin //minus function
 A_temp = R;
 B_temp = B_reg;
 S_temp = 1'b1;
end
else if (current_state == S011) begin //plus function
 A_temp = Q; //Q = 0
 B_temp = 1'd1;
 S_temp = 1'b0;
end
else begin
 A_temp = 0;
 B_temp = 0;
 S_temp = 0;  
end
end

always @(*) begin
if (current_state == S101)begin
 Done = 1;end
else begin
 Done = 0;
end
end
endmodule


//multiplier
module multiplier(input [15:0] y, input[15:0] x, output reg [31:0] s);
reg [15:0] p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15;
always @(*) begin
    p0 = x[0] ? y : 8'd0; 
    p1 = (x[1] ? y : 8'd0);
    p2 = (x[2] ? y : 8'd0);
    p3 = (x[3] ? y : 8'd0);
    p4 = (x[4] ? y : 8'd0);
    p5 = (x[5] ? y : 8'd0);
    p6 = (x[6] ? y : 8'd0);
    p7 = (x[7] ? y : 8'd0);
    p8 = (x[8] ? y : 8'd0);
    p9 = (x[9] ? y : 8'd0);
    p10 = (x[10] ? y : 8'd0);
    p11 = (x[11] ? y : 8'd0);
    p12 = (x[12] ? y : 8'd0);
    p13 = (x[13] ? y : 8'd0);
    p14 = (x[14] ? y : 8'd0);
    p15 = (x[15] ? y : 8'd0);

    s = p0 
        + (p1 << 4'd1) 
        + (p2 << 4'd2) 
        + (p3 << 4'd3)
        + (p4 << 4'd4)
        + (p5 << 4'd5)
        + (p6 << 4'd6)
        + (p7 << 4'd7)
        + (p8 << 4'd8)
        + (p9 << 4'd9)
        + (p10 << 4'd10)
        + (p11 << 4'd11)
        + (p12 << 4'd12)
        + (p13 << 4'd13)
        + (p14 << 4'd14)
        + (p15 << 4'd15);
end
endmodule

//exp
module myexp(output reg[15:0] O, output reg Done,  input [15:0] A, B, input Load,Clk,Reset);
reg [15:0] A_temp, B_temp, A_reg, B_reg;
reg [2:0] current_state, next_state;
wire [15:0] s_temp;
multiplier mymulti(A_temp, B_temp, s_temp);
parameter [2:0] S0 = 3'b000, S1 = 3'b001, S2 = 3'b010, S3 = 3'b011, S4 = 3'b100;

always @(posedge Clk) begin 
 if (Reset) current_state <= S0;
 else current_state <= next_state;
end

always @(*) begin
  case (current_state)
  S0: if(Load) next_state = S1; 
      else next_state = S0;
  S1: if(B_reg==1) next_state = S4;
      else next_state = S2;
  S2: next_state = S3;
  S3: if(B_reg==1) next_state = S4;
      else next_state = S1;
  S4: next_state = S0;
  default: next_state = S0;
  endcase
end

always @(posedge Clk) begin
 if (Reset) begin
  A_reg = 0;
 end
 else if ((current_state == S0) && (Load == 1))begin
  A_reg <= A;
 end
end

always @(posedge Clk) begin
 if (Reset) begin
  B_reg = 0;
 end
 else if ((current_state == S0) && (Load == 1))begin
  B_reg <= B;
 end
 else if (current_state == S2) begin
  B_reg = B_reg - 1; //combinational logic B, do we need distinguish?
  // we make B_reg decrement here, is it good for state machine to make judge=1 !B_reg
 end
end

always @(posedge Clk) begin
if (Reset) begin
 O <= 0;
end
else if ((current_state == S1) && (B_reg == 1))begin
 O <= A_reg;
 end
else if (current_state == S0)begin
 O <= A;
 end
else if (current_state == S2)begin
 O <= s_temp;
 end
end 	
 
always @(*) begin
if (current_state == S2)begin //multiply
 A_temp = O;
 B_temp = A_reg;
end
else begin
 A_temp = 0;
 B_temp = 0;
end
end
 
always @(*) begin
if (current_state == S4)begin
 Done = 1;end
else begin
 Done = 0;
end
end
endmodule

/***************************************************
----------------------alu mod-----------------------
***************************************************/
module alu(
input [15:0] A,
input [15:0] B,
input [2:0] opALU,
output reg [15:0] Rout,
input Reset,
input Clk,
input wire Load,
input wire Load2, //divider input Load
output wire Done,
output wire Done2);


wire [15:0] addO, subO, xorO, notO, divO, expO; //output
wire [31:0] multO;//multi output 
wire Cout1, Cout2; //add or sub cout
wire [15:0] divR; //divider reminder


//assign Done = (opALU == 3'd5 || 3'd6);
//assign Done2 = (opALU == 3'd7);  
//assign Load = (opALU == 3'd5 || 3'd6);
//assign Load2 = (opALU == 3'd7); 
 my16bitsmuxxor my16muxxor(xorO, A, B); //0
 my16bitaddsub_gate my16add(addO, Cout1, A, B, 1'b0); //1 add
 my16bitaddsub_gate my16sub(subO, Cout2, A, B, 1'b1); //2 sub
 my16bitmuxnot my16muxnot(notO, A); //3 my16bitmuxnot(output [15:0]y, input [15:0] a);
 multiplier my16mul(B, A, multO); //4
 my16bitdivider my16div(divO, divR, Done, A, B, Load, Clk, Reset);//5 ,6
 myexp mye(expO, Done2, A, B, Load2,Clk,Reset); //Do we need create different Load, Clk, Reset?
 
 always @ (*) 
 begin
 if (opALU == 0)begin
  Rout = xorO;
  end
 else if (opALU == 3'd1)begin
  Rout = addO;
  end
 else if (opALU == 3'd2)begin
  Rout = subO;
  end
 else if (opALU == 3'd3)begin
  Rout = notO;
  end
 else if (opALU == 3'd4)begin
  Rout = multO; 
  end
 else if (opALU == 3'd5)begin
  Rout = divO;
  end
 else if (opALU == 3'd6)begin
  Rout = divR;
  end
 else if(opALU == 3'd7)begin
  Rout = expO; 
  end
end       
endmodule


/***************************************************
----------------------ctr mod-----------------------
***************************************************/
module ctr (
			clk,
			rst,
			zflag,
			opcode,
			muxPC,
			muxMAR,
			muxACC,
			loadMAR,
			loadPC,
			loadACC,
			loadMDR,
			loadIR,
			opALU,
			MemRW,
			load_div,
			rst_div, //divder load and rst
			done_div, //done_exp, load_exp
			load_exp,//proj2
			done_exp
			);//proj2			
input clk;
input rst;
input zflag;
input [7:0]opcode;
output reg rst_div; //divider rst
input done_div; //divider done
input done_exp;
output reg load_div;  //divder load
output reg load_exp;
output reg muxPC;
output reg muxMAR;
output reg muxACC;
output reg loadMAR;
output reg loadPC;
output reg loadACC;
output reg loadMDR;
output reg loadIR;
output reg [2:0] opALU;
output reg MemRW;
reg mod;

parameter [3:0] op_add = 4'h1, 
	op_sub = 4'h2, 
	op_mul = 4'h3,
	op_div = 4'h4,
	op_or = 4'h5,
	op_jump = 4'h6,
	op_jumpz = 4'h7,
	op_store = 4'h8,
	op_load = 4'h9,
	op_mod = 4'hA,
	op_exp = 4'hB; //proj2
parameter [5:0] Fetch_1 = 6'd0,
	Fetch_2 = 6'd1,
	Fetch_3 = 6'd2,
	Decode = 6'd3,
	ExecADD_1 = 6'd4,
	ExecOR_1 = 6'd5,
	ExecLoad_1 = 6'd6,
	ExecStore_1 = 6'd7,
	ExecJump = 6'd8,
	ExecADD_2 = 6'd9,
	ExecOR_2 = 6'd10,
	ExecLoad_2 = 6'd11,
	ExecMUL_1 = 6'd12, //
	ExecMUL_2 = 6'd13,
	ExecSUB_1 = 6'd14,
	ExecSUB_2 = 6'd15,
	ExecDIV_0 = 6'd16,
	ExecDIV_1 = 6'd17,
	ExecDIV_2 = 6'd18,
	ExecDIV_3 = 6'd19,
	ExecDIV_4 = 6'd20,
	ExecDIV_5 = 6'd21,
	ExecEXP_0 = 6'd22,
	ExecEXP_1 = 6'd23,
	ExecEXP_2 = 6'd24,
	ExecEXP_3 = 6'd25;
	//proj2

reg [5:0]current_state; //add by myself
reg [5:0]next_state;//add by myself

always @(posedge clk) 
begin
	if (rst) 
		current_state <= Fetch_1;
	else 
		current_state <= next_state;
end

always @(*) 
begin
  case (current_state)
		Fetch_1: next_state = Fetch_2;
		Fetch_2: next_state = Fetch_3;
		Fetch_3: next_state = Decode;
		Decode: 
			if (opcode == op_add) 
				next_state = ExecADD_1;
			else if (opcode == op_sub) 
				next_state = ExecSUB_1;
			else if (opcode == op_or) 
				next_state = ExecOR_1;// xor, or?
			else if (opcode == op_mul) 
				next_state = ExecMUL_1;
			else if ((opcode == op_div) || (opcode == op_mod)) begin //hw8
			     mod = (opcode == op_mod); //hw8
				next_state = ExecDIV_0;
				end
			else if (opcode == op_exp)
			  next_state = ExecEXP_0; //proj2
			else if (opcode == op_load) 
				next_state = ExecLoad_1;
			else if (opcode == op_store) 
				next_state = ExecStore_1;
			else if ((opcode == op_jump) || ((opcode == op_jumpz) && zflag)) 
				next_state = ExecJump;
			else if ((opcode == op_jumpz) && !zflag) 
				next_state = Fetch_1;
			else 
				next_state = Fetch_1;
		ExecADD_1: 
			next_state = ExecADD_2;
		ExecADD_2: 
			next_state = Fetch_1;
		ExecSUB_1: 
			next_state = ExecSUB_2;
		ExecSUB_2: 
			next_state = Fetch_1;
		ExecOR_1: 
			next_state = ExecOR_2;
		ExecOR_2: 
			next_state = Fetch_1;
		ExecLoad_1: 
			next_state = ExecLoad_2;
		ExecLoad_2:
			next_state = Fetch_1;
		ExecStore_1: 
			next_state = Fetch_2;
		ExecJump: 
			next_state = Fetch_1;
		ExecMUL_1: 
			next_state = ExecMUL_2;
		ExecMUL_2: 
			next_state = Fetch_1;
		ExecDIV_0: 
			next_state = ExecDIV_1;
		ExecDIV_1: 
			next_state = ExecDIV_2;
		ExecDIV_2: 
			next_state = (done_div) ? ExecDIV_3 : ExecDIV_2;
		ExecDIV_3: 
			next_state = Fetch_1;
		ExecEXP_0: //proj2
			next_state = ExecEXP_1;
		ExecEXP_1: 
			next_state = ExecEXP_2;
		ExecEXP_2: 
			next_state = (done_exp) ? ExecEXP_3 : ExecEXP_2;
		ExecEXP_3: 
			next_state = Fetch_1;//proj2
  endcase  
end
   
always @(*) 
	begin
		muxMAR = (current_state == Decode)? 1'b1: 1'b0;
		muxPC = (current_state == ExecJump)? 1'b1: 1'b0;
		loadPC = ((current_state == Fetch_1)||(current_state == ExecJump))? 1'b1: 1'b0;
		loadMAR = ((current_state == Fetch_1)||(current_state == Decode))? 1'b1: 1'b0;
		MemRW = (current_state == ExecStore_1)? 1'b1: 1'b0;
		loadMDR = ((current_state == Fetch_2)||(current_state == ExecADD_1)||(current_state == ExecOR_1)||(current_state == ExecLoad_1)||(current_state == ExecMUL_1)||(current_state == ExecSUB_1)||(current_state == ExecDIV_0)||(current_state == ExecEXP_0))? 1'b1: 1'b0;//proj2
		loadIR = (current_state == Fetch_3)? 1'b1: 1'b0;
		loadACC = ((current_state == ExecADD_2)||(current_state == ExecOR_2)||(current_state == ExecLoad_2)||(current_state == ExecMUL_2)||(current_state == ExecSUB_2)||(current_state == ExecDIV_3)||(current_state == ExecEXP_3))? 1'b1: 1'b0;//proj2
		muxACC = (current_state == ExecLoad_2)? 1'b1: 1'b0;
		rst_div = ((current_state == ExecDIV_1)||(current_state == ExecEXP_1))? 1'b1: 1'b0; //proj2
		load_div = (current_state == ExecDIV_1)? 1'b1: 1'b0; //proj2    
    load_exp = (current_state == ExecEXP_1)? 1'b1: 1'b0;
	end


always @ (*) 
 begin
 if (current_state == ExecOR_2)
	begin
		opALU = 3'd0;
	end
 else if (current_state == ExecADD_2)
	begin
		opALU = 3'd1;
	end 
 else if (current_state == ExecSUB_2)
	begin
		opALU = 3'd2;
	end
 else if (current_state == ExecMUL_2)
	begin
		opALU = 3'd4;
	end
 else if (current_state == ExecDIV_3)
	begin
		opALU = mod ? 3'd6:3'd5;		// 3'd6 = 110
	end
 else if (current_state == ExecEXP_3)
	begin
		opALU = 3'd7;
	end
end 
endmodule

/***************************************************
----------------------path mod----------------------
***************************************************/  
module datapath(
		clk,
		rst,
		muxPC,
		muxMAR,
		muxACC,
		loadMAR,
		loadPC,
		loadACC,
		loadMDR,
		loadIR,
		opALU,
		zflag,
		opcode,
		MemAddr,
		MemD,
		MemQ,
		Done_s,
		Load_s,
		Done_e,//proj2
		Load_e//proj2
);

input clk;
input rst;
input muxPC;
input muxMAR;
input muxACC;
input loadMAR;
input loadPC;
input loadACC;
input loadMDR;
input loadIR;
input [2:0] opALU;
input Load_s;
input Load_e; //proj2

output zflag;
output [7:0]opcode;
output [7:0]MemAddr;
output [15:0]MemD;
output Done_s;
output Done_e;

input [15:0]MemQ;
reg [7:0]PC_next;
reg [15:0]IR_next;
reg [15:0]ACC_next;
reg [15:0]MDR_next;
reg [7:0]MAR_next;
reg zflag_next;
wire [7:0]PC_reg;
wire [15:0]IR_reg;
wire [15:0]ACC_reg;
wire [15:0]MDR_reg;
wire [7:0]MAR_reg;
wire zflag_reg;
wire [15:0]ALU_out;
reg [15:0] A;
reg [15:0] B;
wire zflag;
//reg opcode;
//reg MemAddr;
//reg MemD;

alu myalu(
		.A(A),
		.B(B),
		.opALU(opALU),
		.Rout(ALU_out),
		.Reset(rst),
		.Clk(clk),
		.Load(Load_s),
		.Load2(Load_e),
		.Done(Done_s),
		.Done2(Done_e)
		);

registers myreg(
		clk,
		rst,
		PC_reg,
		PC_next,
		IR_reg,
		IR_next,
		ACC_reg,
		ACC_next,
		MDR_reg,
		MDR_next,
		MAR_reg,
		MAR_next,
		zflag_reg,
		zflag_next
		);

always @(*)
	begin
		A = ACC_reg;
		B = MDR_reg;
	end

always @(*)
 begin
		PC_next = (loadPC)? ((muxPC)? IR_reg[15:8]: PC_reg + 1): PC_reg;
		IR_next = (loadIR)? MDR_reg: IR_reg;
		ACC_next = (loadACC)? ((muxACC)? MDR_reg: ALU_out): ACC_reg;
		MDR_next = (loadMDR)? MemQ: MDR_reg;
		MAR_next = (loadMAR)? ((muxMAR)?IR_reg[15:8]: PC_reg): MAR_reg;
		zflag_next = (ACC_reg)? 1'b0 : 1'b1;
 end

assign zflag = zflag_reg;
assign opcode = IR_reg[7:0];
assign MemAddr = MAR_reg;
assign MemD = ACC_reg;
endmodule

/***************************************************
----------------------ram mod-----------------------
***************************************************/
module ram(
		input we, 
		input [15:0] d, 
		output reg [15:0] q, 
		input [7:0]	addr
		);
		
reg [15:0] mem256x16 [0:255];	

always @(*)
 begin 
 if (we)
		mem256x16[addr] = d; 	//write
 
 else 
		q = mem256x16[addr];	//read
 end  
endmodule

/***************************************************
----------------------reg mod-----------------------
***************************************************/
module registers(
		clk,
		rst,
		PC_reg,
		PC_next,
		IR_reg,
		IR_next,
		ACC_reg,
		ACC_next,
		MDR_reg,
		MDR_next,
		MAR_reg,
		MAR_next,
		Zflag_reg,
		zflag_next
);

input wire clk;
input wire rst;
output reg [7:0]PC_reg;
input wire [7:0]PC_next;
output reg [15:0]IR_reg;
input wire [15:0]IR_next;
output reg [15:0]ACC_reg;
input wire [15:0]ACC_next;
output reg [15:0]MDR_reg;
input wire [15:0]MDR_next;
output reg [7:0]MAR_reg;
input wire [7:0]MAR_next;
output reg Zflag_reg;
input wire zflag_next;

always @(posedge clk) 
begin
 if (rst) 
	begin
		PC_reg = 8'b0;
		IR_reg = 16'b0;
		ACC_reg = 16'b0;
		MDR_reg = 16'b0;
		MAR_reg = 8'b0;
		Zflag_reg = 1'b0;
	end
 else 
	begin
		PC_reg <= PC_next;
		IR_reg <= IR_next;
		ACC_reg <= ACC_next;
		MDR_reg <= MDR_next;
		MAR_reg <= MAR_next;
		Zflag_reg <= zflag_next;
	end
end 
endmodule

/***************************************************
----------------------tb mod------------------------
***************************************************/
module prj2_tb();
reg clk;
reg rst;
wire MemRW_IO;
wire [7:0]MemAddr_IO;
wire [15:0]MemD_IO;

prj2 dut(clk, rst, MemRW_IO, MemAddr_IO, MemD_IO);		


always
#5 clk = !clk;
initial begin
$readmemh("Prj2.memlist", prj2_tb.dut.ram_ins.mem256x16);
clk=1'b0;
rst=1'b1;
#20 rst=1'b0;
#40000 //might need to be very large
$display("Final value\n");
$display("0x000e %d\n",prj2_tb.dut.ram_ins.mem256x16[16'h000e]);
$finish;
end
endmodule



