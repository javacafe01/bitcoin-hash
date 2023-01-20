module bitcoin_hash(
	input logic clk, reset_n, start,
	input logic [15:0] header_addr, hash_out_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] memory_addr,
	output logic [31:0] memory_write_data,
	input logic [31:0] memory_read_data
);

// VARIABLES
				
parameter num_nonces = 16;
assign mem_clk = clk; 
enum logic [2:0] {IDLE, READSTART, READ, READEND, COMP, WRITE} state;
logic [1:0] switch, read_switch, write_switch;
logic [31:0] a,b,c,d,e,f,g,h,temp,wt,nonce, w[16], H[8], t, t2, cycles;
logic [15:0] read_shift;


parameter int k[0:63] = '{
   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};


// FUNCTIONS

function logic [31:0] rightrotate(input logic [31:0] x, input logic [7:0] r);
  rightrotate = (x >> r) | (x << (32-r));
endfunction 


function logic [255:0] comp_sha256(input logic [31:0] a,b,c,d,e,f,g,h,w,k);
  logic [31:0] s0, s1, ch, maj, t1, t2;
  s0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
  maj = (a & b) ^ (a & c) ^ (b & c);
  t2 = maj + s0;
  s1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
  ch = (e & f) ^ ((~e) & g);
  t1 = ch + s1 + h + k + w;
  
  comp_sha256 = {t1+t2, a, b, c, d+t1, e, f, g};
endfunction

function logic [31:0] getw;
  logic [31:0] s0, s1;
  s0 = rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
  s1 = rightrotate(w[14],17)^rightrotate(w[14],19)^(w[14]>>10);
  getw = w[0]+s0+w[9]+s1;
endfunction 


// SEQ LOGIC


always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) state <= IDLE;
  else begin
    case (state)
      IDLE: begin
        if (start) begin
            mem_we <= 0;
            memory_addr <= header_addr;
            read_shift <= 1;
            t <= 0;
            t2 <= 0;
            nonce <= 0;
            switch <= 0;
            read_switch <= 0;
            write_switch <= 0;
            cycles = 1;

            H[0] <= 32'h6a09e667;
            H[1] <= 32'hbb67ae85;
            H[2] <= 32'h3c6ef372;
            H[3] <= 32'ha54ff53a;
            H[4] <= 32'h510e527f;
            H[5] <= 32'h9b05688c;
            H[6] <= 32'h1f83d9ab;
            H[7] <= 32'h5be0cd19;
            state <= READSTART;
        end
      end
		
      READSTART: begin
        if (read_switch == 0) begin
          memory_addr <= header_addr + read_shift;
          read_shift <= read_shift + 1;
          read_switch <= 1;
        end
        else begin
          memory_addr <= header_addr + read_shift;
          read_shift <= read_shift + 1;
          w[t2] <= memory_read_data;
          t2 <= t2 + 1;
          state <= READ;
        end
      end

      READ: begin
        w[t2] <= memory_read_data;
        t2 <= t2 + 1;
        if (read_shift == 21) begin
          state <= COMP;
          w[3] <= nonce;
          for (int i = 4; i < 16; i++) begin
            if (i == 4) begin
              w[i] <= 32'h80000000;
            end
            else if (i == 15) begin
              w[i] <= 32'd640;
            end
            else begin
              w[i] <= 32'h00000000;
            end
          end
          wt <= w[0];
          {a, b, c, d, e, f, g, h} <= {H[0], H[1], H[2], H[3], H[4], H[5], H[6], H[7]};
          t <= t + 1;
          switch <= 1;
        end
        else if (t2 == 15) begin
          state <= COMP;
          t2 <= 0;
		      wt <= w[0];
          t <= t + 1;
          {a, b, c, d, e, f, g, h} <= {H[0], H[1], H[2], H[3], H[4], H[5], H[6], H[7]};
        end
        else begin
          state <= READSTART;
          memory_addr <= header_addr + read_shift;
          read_shift <= read_shift + 1;
        end
      end
		
      READEND: begin
        for (int i = 8; i < 16; i++) begin
            if (i == 8) begin
              w[i] <= 32'h80000000;
            end
            else if (i == 15) begin
              w[i] <= 32'd256;
            end
            else begin
              w[i] <= 32'h00000000;
            end
        end
        state <= COMP;
        wt <= w[0];
        t <= t + 1;
        {a, b, c, d, e, f, g, h} <= {H[0], H[1], H[2], H[3], H[4], H[5], H[6], H[7]};
      end  
		
      COMP: begin

        if (t < 65) begin
          if (t < 16) begin
            wt <= w[t];
          end
          else begin
            wt <= getw;
            for (int i = 0; i<15;i++) begin
              w[i] <= w[i+1];
            end
            w[15] <= getw;
          end
          {a, b, c, d, e, f, g, h} <= comp_sha256(a, b, c, d, e, f, g, h, wt, k[t-1]);

          t <= t + 1;
        end
        else begin 
          if (switch == 1) begin
            state <= READEND;
            t <= 0; 
            {w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7]} <= {H[0]+a, H[1]+b, H[2]+c, H[3]+d, H[4]+e, H[5]+f, H[6]+g, H[7]+h};
            H[0] <= 32'h6a09e667;
            H[1] <= 32'hbb67ae85;
            H[2] <= 32'h3c6ef372;
            H[3] <= 32'ha54ff53a;
            H[4] <= 32'h510e527f;
            H[5] <= 32'h9b05688c;
            H[6] <= 32'h1f83d9ab;
            H[7] <= 32'h5be0cd19;
            switch <= 0;
          end
          else if (read_shift < 21) begin
            state <= READSTART;
            t <= 0;
            {H[0], H[1], H[2], H[3], H[4], H[5], H[6], H[7]} <= {H[0]+a, H[1]+b, H[2]+c, H[3]+d, H[4]+e, H[5]+f, H[6]+g, H[7]+h};
            read_switch <= 0;
          end
          else begin
            {H[0], H[1], H[2], H[3], H[4], H[5], H[6], H[7]} <= {H[0]+a, H[1]+b, H[2]+c, H[3]+d, H[4]+e, H[5]+f, H[6]+g, H[7]+h};
            state <= WRITE;
            
          end
        end
        cycles <= cycles + 1;
      end

      WRITE: begin
        if (write_switch == 0) begin
          mem_we <= 1;
          memory_addr <= hash_out_addr + nonce;
          memory_write_data <= H[0];
          nonce <= nonce + 1;
          write_switch <= 1;
        end
        else if (nonce == num_nonces) begin
          done <= 1;
          state <= IDLE;
        end
        else begin
          mem_we <= 0;
          state <= READSTART;
          read_shift <= 1;
          t <= 0;
          t2 <= 0;
          switch <= 0;
          read_switch <= 0;
          write_switch <= 0;
          memory_addr <= header_addr;
          H[0] <= 32'h6a09e667;
          H[1] <= 32'hbb67ae85;
          H[2] <= 32'h3c6ef372;
          H[3] <= 32'ha54ff53a;
          H[4] <= 32'h510e527f;
          H[5] <= 32'h9b05688c;
          H[6] <= 32'h1f83d9ab;
          H[7] <= 32'h5be0cd19;
        end
        cycles <= cycles + 1;
      end
  endcase
  end
end
endmodule 