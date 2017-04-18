----------------------------------------------------------------------------------
-- Description:
--
-- Additional Comments:
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

entity Computer is
  port (
    sys_clk : in std_logic;
    reset   : in std_logic;
    d_reg   : out std_logic_vector(15 downto 0));
end entity Computer;

architecture behavior of Computer is
  component CPU is
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
  end component CPU;

  component blk_mem_gen_inst_rom is
    port (
      clka  : IN STD_LOGIC;
      addra : IN STD_LOGIC_VECTOR(14 DOWNTO 0);
      douta : OUT STD_LOGIC_VECTOR(15 DOWNTO 0));
  end component blk_mem_gen_inst_rom;

  component blk_mem_gen_data_ram is
    port (
        clka  : IN  STD_LOGIC;
        wea   : IN  STD_LOGIC_VECTOR(0 DOWNTO 0);
        addra : IN  STD_LOGIC_VECTOR(13 DOWNTO 0);
        dina  : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
        douta : OUT STD_LOGIC_VECTOR(15 DOWNTO 0));
  end component blk_mem_gen_data_ram;

  constant cpu_count : integer := 3;
  signal cpu_counter : unsigned(1 downto 0);
  signal cpu_clk     : std_logic;

  signal ram_data_out : std_logic_vector(15 downto 0);
  signal outM         : std_logic_vector(15 downto 0);
  signal writeM       : std_logic;
  signal addressM     : std_logic_vector(14 downto 0);
  signal pc_addr      : std_logic_vector(14 downto 0);

  signal inM          : std_logic_vector(15 downto 0);
  signal instruction  : std_logic_vector(15 downto 0);

  signal ram_write_en : std_logic_vector(0 downto 0);
begin

  ram_write_en <= "1" when (writeM = '1' and unsigned(addressM) < 16384) else "0";

  data_ram : blk_mem_gen_data_ram
    port map(
      clka  => sys_clk,
      wea   => ram_write_en,
      addra => addressM(13 downto 0),
      dina  => outM,
      douta => inM);

  inst_rom : blk_mem_gen_inst_rom
    port map(
      clka  => sys_clk,
      addra => pc_addr,
      douta => instruction);

  CPUb : CPU
    port map(
      clk         => cpu_clk,
      inM         => inM,
      instruction => instruction,
      reset       => reset,
      outM        => outM,
      writeM      => writeM,
      addressM    => addressM,
      pc_addr     => pc_addr,
      d_reg_out   => d_reg);
  
  cpu_clk_proc : process(sys_clk, reset)
  begin
    if (reset = '1') then
      cpu_clk <= '0';
      cpu_counter <= to_unsigned(0, cpu_counter'length);
    elsif rising_edge(sys_clk) then
      if (cpu_counter = cpu_count) then
        cpu_clk <= not cpu_clk;
        cpu_counter <= (others => '0');
      else
        cpu_counter <= cpu_counter + 1;
      end if;
    end if;
  end process cpu_clk_proc;
      
end behavior;
