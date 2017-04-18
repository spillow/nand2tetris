----------------------------------------------------------------------------------
-- Dependencies:
--
-- Additional Comments:
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity CPU is
  port (
    clk         : in  std_logic;
    inM         : in  std_logic_vector(15 downto 0);
    instruction : in  std_logic_vector(15 downto 0);
    reset       : in  std_logic;
    outM        : out std_logic_vector(15 downto 0);
    writeM      : out std_logic;
    addressM    : out std_logic_vector(14 downto 0);
    pc_addr     : out std_logic_vector(14 downto 0);
    d_reg_out   : out std_logic_vector(15 downto 0));
end entity CPU;

architecture Behavioral of CPU is
  signal is_c_inst : std_logic;         -- C-instruction?
  signal is_a_inst : std_logic;         -- A-instruction?

  -- left or right column of computation table?
  signal comp_a : std_logic;

  signal dest_a : std_logic;
  signal dest_d : std_logic;
  signal dest_m : std_logic;

  signal jump      : std_logic_vector(2 downto 0);
  signal alu_ctrls : std_logic_vector(5 downto 0);

  signal d_reg      : std_logic_vector(15 downto 0);
  signal d_value    : std_logic_vector(15 downto 0);
  signal d_write_en : std_logic;

  signal a_reg      : std_logic_vector(15 downto 0);
  signal a_value    : std_logic_vector(15 downto 0);
  signal a_write_en : std_logic;

  signal alu_output : std_logic_vector(15 downto 0);

  signal pc_output  : std_logic_vector(15 downto 0);

  signal am_select  : std_logic_vector(15 downto 0);

  signal pc_load    : std_logic;

  signal zr         : std_logic;
  signal ng         : std_logic;

  component ALU is
    port(
      x    : in  std_logic_vector(15 downto 0);
      y    : in  std_logic_vector(15 downto 0);
      zx   : in  std_logic;
      nx   : in  std_logic;
      zy   : in  std_logic;
      ny   : in  std_logic;
      f    : in  std_logic;
      no   : in  std_logic;
      outb : out std_logic_vector(15 downto 0);
      zr   : out std_logic;
      ng   : out std_logic);
  end component ALU;
  
  component PC is
    port (
      clk    : in  std_logic;
      input  : in  std_logic_vector(15 downto 0);
      load   : in  std_logic;
      inc    : in  std_logic;
      reset  : in  std_logic;
      output : out std_logic_vector(15 downto 0));
  end component PC;
begin
  is_c_inst <= instruction(15);
  is_a_inst <= not instruction(15);
  comp_a    <= instruction(12);
  dest_a    <= instruction(5);
  dest_d    <= instruction(4);
  dest_m    <= instruction(3);
  jump      <= instruction(2 downto 0);
  alu_ctrls <= instruction(11 downto 6);

  d_write_en <= is_c_inst and dest_d;
  d_value    <= alu_output;

  a_write_en <= is_a_inst or dest_a;
  a_value    <= instruction when is_a_inst = '1' else
                alu_output;

  addressM   <= a_reg(14 downto 0);
  writeM     <= is_c_inst and dest_d;
  outM       <= alu_output;

  am_select  <= a_reg when comp_a = '0' else inM;

  d_reg_out  <= d_reg;

  ALUb : ALU
    port map(
      x    => d_reg,
      y    => am_select,
      zx   => alu_ctrls(5),
      nx   => alu_ctrls(4),
      zy   => alu_ctrls(3),
      ny   => alu_ctrls(2),
      f    => alu_ctrls(1),
      no   => alu_ctrls(0),
      outb => alu_output,
      zr   => zr,
      ng   => ng);

  PCb : PC
    port map(
      clk    => clk,
      input  => a_reg,
      load   => pc_load,
      reset  => reset,
      inc    => '1',
      output => pc_output);

  pc_addr <= pc_output(14 downto 0);

  reg_update : process(clk)
  begin
    if rising_edge(clk) then
      if (a_write_en = '1') then
        a_reg <= a_value;
      end if;
      if (d_write_en = '1') then
        d_reg <= d_value;
      end if;
    end if;
  end process reg_update;

  pc_loadb : process(jump, zr, ng)
  begin
    pc_load <= '0';
    case jump is
      when "000"  => pc_load <= '0';                    -- null
      when "001"  => pc_load <= (not ng) and (not zr);  -- JGT
      when "010"  => pc_load <= zr;                     -- JEQ
      when "011"  => pc_load <= not ng;                 -- JGE
      when "100"  => pc_load <= ng;                     -- JLT
      when "101"  => pc_load <= not zr;                 -- JNE
      when "110"  => pc_load <= zr or ng;               -- JLE
      when "111"  => pc_load <= '1';                    -- JMP
      when others => pc_load <= '0';
    end case;
  end process pc_loadb;

end Behavioral;
