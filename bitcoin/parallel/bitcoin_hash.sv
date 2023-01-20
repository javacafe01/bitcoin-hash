module bitcoin_hash(
	input logic clk, reset_n, start,
	input logic [15:0] header_addr, hash_out_addr,
	output logic done, mem_clk, mem_we,
   	output logic [15:0] memory_addr,
   	output logic [31:0] memory_write_data,
   	input logic [31:0] memory_read_data
);

// Variables

parameter num_nonces = 16;

assign mem_clk = clk; 
enum logic [2:0] {IDLE, STARTREAD, READ, READEND, COMP, WRITE} state;
logic [31:0] a[num_nonces], b[num_nonces], c[num_nonces], d[num_nonces], e[num_nonces], f[num_nonces], g[num_nonces], h[num_nonces], wt[num_nonces], w[num_nonces][16], H[num_nonces][8], nonce, t, t2;
logic [15:0] read_set;
logic [1:0] switch, startswitch;

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

// FUNCTION

function logic [31:0] rightrotate(input logic [31:0] x, input logic [7:0] r);
  rightrotate = (x >> r) | (x << (32-r));
endfunction 

// avoids needing to add other file
function logic [255:0] get_sha256(input logic [31:0] a,b,c,d,e,f,g,h,w,k);
  logic [31:0] s0, s1, ch, maj, t1, t2;
  s0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
  maj = (a & b) ^ (a & c) ^ (b & c);
  t2 = maj + s0;
  s1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
  ch = (e & f) ^ ((~e) & g);
  t1 = ch + s1 + h + k + w;
  
  get_sha256 = {t1+t2, a, b, c, d+t1, e, f, g};
endfunction

function logic [31:0] getw(input int n);
  logic [31:0] s0, s1;
  s0 = rightrotate(w[n][1],7)^rightrotate(w[n][1],18)^(w[n][1]>>3);
  s1 = rightrotate(w[n][14],17)^rightrotate(w[n][14],19)^(w[n][14]>>10);
  getw = w[n][0]+s0+w[n][9]+s1;
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
            read_set <= 1;
            t <= 0;
            t2 <= 0; 
            nonce <= 0;
            switch <= 0;
            startswitch <= 0;
				

            for (int n = 0; n < num_nonces; n++) begin
              H[n][0] <= 32'h6a09e667;
              H[n][1] <= 32'hbb67ae85;
              H[n][2] <= 32'h3c6ef372;
              H[n][3] <= 32'ha54ff53a;
              H[n][4] <= 32'h510e527f;
              H[n][5] <= 32'h9b05688c;
              H[n][6] <= 32'h1f83d9ab;
              H[n][7] <= 32'h5be0cd19;
            end
            state <= STARTREAD;
        end
      end
		
      STARTREAD: begin
        if (startswitch == 0) begin
          memory_addr <= header_addr + read_set;
          read_set <= read_set + 1;
          startswitch <= 1;
        end
        else begin
          memory_addr <= header_addr + read_set; 
          read_set <= read_set + 1;
          t2 <= t2 + 1; 
          state <= READ;
          for (int n=0; n < num_nonces; n++) begin
            w[n][t2] <= memory_read_data; 
          end
        end
      end
		
      READ: begin 
        for (int n = 0; n < num_nonces; n++) begin
            w[n][t2] <= memory_read_data;
        end
        t2 <= t2 + 1;
        if (read_set == 21) begin
          for (int n = 0; n < num_nonces; n++) begin
            w[n][t2] <= memory_read_data;
            state <= COMP;
            w[n][3] <= n;
            for (int i = 4; i < 16; i++) begin
              if (i == 4) begin
                w[n][i] <= 32'h80000000;
              end
              else if (i == 15) begin
                w[n][i] <= 32'd640;
              end
              else begin
                w[n][i] <= 32'h00000000;
              end
            end
            wt[n] <= w[n][0];
            {a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n]} <= {H[n][0], H[n][1], H[n][2], H[n][3], H[n][4], H[n][5], H[n][6], H[n][7]};
          end
          t <= t + 1;
          switch <= 1;
        end
        else if (t2 == 15) begin
          state <= COMP;
          t2 <= 0;
          t <= t + 1;
          for (int n = 0; n < num_nonces; n++) begin
            wt[n] <= w[n][0];
            {a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n]} <= {H[n][0], H[n][1], H[n][2], H[n][3], H[n][4], H[n][5], H[n][6], H[n][7]};
          end
        end
        else begin
          state <= STARTREAD;
          memory_addr <= header_addr + read_set;
          read_set <= read_set + 1;
        end
      end
		
      READEND: begin
        for (int n = 0; n < num_nonces; n++) begin
          for (int i = 8; i < 16; i++) begin
              if (i == 8) begin
                w[n][i] <= 32'h80000000;
              end
              else if (i == 15) begin
                w[n][i] <= 32'd256;
              end
              else begin
                w[n][i] <= 32'h00000000;
              end
          end
          state <= COMP;
          wt[n] <= w[n][0];
          {a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n]} <= {H[n][0], H[n][1], H[n][2], H[n][3], H[n][4], H[n][5], H[n][6], H[n][7]};
        end
        t <= t + 1;
      end  
		
      COMP: begin
        if (t < 65) begin
          for (int n = 0; n < num_nonces; n++) begin
            if (t < 16) begin
              wt[n] <= w[n][t];
            end
            else begin
              wt[n] <= getw(n);
              for (int i = 0; i<15;i++) begin
                w[n][i] <= w[n][i+1];
              end
              w[n][15] <= getw(n);
            end
            {a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n]} <= get_sha256(a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n], wt[n], k[t-1]);
          end
          t <= t + 1;
        end
        else begin 
          if (switch == 1) begin
            state <= READEND;
            t <= 0; 
            for (int n = 0; n < num_nonces; n++) begin
              {w[n][0], w[n][1], w[n][2], w[n][3], w[n][4], w[n][5], w[n][6], w[n][7]} <= {H[n][0]+a[n], H[n][1]+b[n], H[n][2]+c[n], H[n][3]+d[n], H[n][4]+e[n], H[n][5]+f[n], H[n][6]+g[n], H[n][7]+h[n]};
              H[n][0] <= 32'h6a09e667;
              H[n][1] <= 32'hbb67ae85;
              H[n][2] <= 32'h3c6ef372;
              H[n][3] <= 32'ha54ff53a;
              H[n][4] <= 32'h510e527f;
              H[n][5] <= 32'h9b05688c;
              H[n][6] <= 32'h1f83d9ab;
              H[n][7] <= 32'h5be0cd19;
            end
            switch <= 0;
          end
			 
          else if (read_set < 21) begin
            state <= STARTREAD;
            t <= 0;
            for (int n = 0; n < num_nonces; n++) begin
              {H[n][0], H[n][1], H[n][2], H[n][3], H[n][4], H[n][5], H[n][6], H[n][7]} <= {H[n][0]+a[n], H[n][1]+b[n], H[n][2]+c[n], H[n][3]+d[n], H[n][4]+e[n], H[n][5]+f[n], H[n][6]+g[n], H[n][7]+h[n]};
            end
            startswitch <= 0;
          end
          else begin
            for (int n = 0; n < num_nonces; n++) begin
              {H[n][0], H[n][1], H[n][2], H[n][3], H[n][4], H[n][5], H[n][6], H[n][7]} <= {H[n][0]+a[n], H[n][1]+b[n], H[n][2]+c[n], H[n][3]+d[n], H[n][4]+e[n], H[n][5]+f[n], H[n][6]+g[n], H[n][7]+h[n]};
            end
            state <= WRITE;
          end
        end
      end

      WRITE: begin
        mem_we <= 1;
        if (nonce <= num_nonces) begin
          switch <= 1;
          memory_addr <= hash_out_addr + nonce;
          memory_write_data <= H[nonce][0];
          nonce <= nonce + 1;
        end
        else begin
          done <= 1;
          state <= IDLE;
        end
      end
  endcase
  end
end
endmodule   
