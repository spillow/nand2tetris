library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

entity test_PC is
end;

architecture test of test_PC is

component PC is
  port (
    clk    : in  std_logic;
    input  : in  std_logic_vector(15 downto 0);
    load   : in  std_logic;
    inc    : in  std_logic;
    reset  : in  std_logic;
    output : out std_logic_vector(15 downto 0));
end component PC;

signal clk    : std_logic                     := '0';
signal input  : std_logic_vector(15 downto 0) := X"FEED";
signal load   : std_logic                     := '0';
signal inc    : std_logic                     := '0';
signal reset  : std_logic                     := '1';
signal output : std_logic_vector(15 downto 0) := X"BEEF";

begin
  dev_to_test : PC
    port map(
      clk    => clk,
      input  => input,
      load   => load,
      inc    => inc,
      reset  => reset,
      output => output);

  clk_stimulus: process
  begin
    wait for 10 ns;
    clk <= not clk;
  end process clk_stimulus;

  stimulus: process
  begin
    for i in std_logic range '0' to '1' loop
      load <= i;
      for j in std_logic range '0' to '1' loop
        inc <= j;
        for k in std_logic range '0' to '1' loop
          reset <= k;

          wait for 40 ns;
        end loop;
      end loop;
    end loop;
  end process stimulus;

end test;
