library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

entity test_Computer is
end;

architecture test of test_Computer is

component Computer is
  port (
    sys_clk : in std_logic;
    reset   : in std_logic;
    d_reg   : out std_logic_vector(15 downto 0));
end component Computer; 

signal clk    : std_logic                     := '0';
signal reset  : std_logic                     := '1';
signal d_reg  : std_logic_vector(15 downto 0) := X"BEEF";

begin
  dev_to_test : Computer
    port map(
      sys_clk => clk,
      reset   => reset,
      d_reg   => d_reg);

  clk_stimulus: process
  begin
    wait for 10 ns;
    clk <= not clk;
  end process clk_stimulus;

  reset_stimulus: process
  begin
    wait for 100 ns;
    reset <= '0';
  end process reset_stimulus;

end test;
