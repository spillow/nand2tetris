----------------------------------------------------------------------------------
-- Description:
--
-- Additional Comments:
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity PC is
  port (
    clk    : in  std_logic;
    input  : in  std_logic_vector(15 downto 0);
    load   : in  std_logic;
    inc    : in  std_logic;
    reset  : in  std_logic;
    output : out std_logic_vector(15 downto 0));
end entity PC;

architecture behavior of PC is
  signal output_tmp : std_logic_vector(15 downto 0);
begin

  output <= output_tmp;

  proc: process(clk, reset)
  begin
    if (reset = '1') then
        output_tmp <= (others => '0');
    elsif rising_edge(clk) then
      if (load = '1') then
        output_tmp <= input;
      elsif (inc = '1') then
        output_tmp <= std_logic_vector(unsigned(output_tmp) + 1);
      end if;
    end if;
  end process proc;

end behavior;
