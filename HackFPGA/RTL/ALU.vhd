----------------------------------------------------------------------------------
-- Description:
--
-- Additional Comments:
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ALU is
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
end entity ALU;

architecture behavior of ALU is
  signal xmod    : std_logic_vector(15 downto 0);
  signal ymod    : std_logic_vector(15 downto 0);
  signal comp    : std_logic_vector(15 downto 0);
  signal neg_out : std_logic_vector(15 downto 0);
begin

  xmodp: process(zx, nx, x)
  begin
    if (zx = '1') then
      xmod <= (others => '0');
    elsif (nx = '1') then
      xmod <= not x;
    else
      xmod <= x;
    end if;
  end process xmodp;

  ymodp: process(zy, ny, y)
  begin
    if (zy = '1') then
      ymod <= (others => '0');
    elsif (ny = '1') then
      ymod <= not y;
    else
      ymod <= y;
    end if;
  end process ymodp;

  comp <= std_logic_vector(unsigned(xmod) + unsigned(ymod)) when f = '1' else
          (xmod and ymod);

  neg_out <= comp when no = '0' else
             not comp;

  zr <= '1' when neg_out = (neg_out'range => '0') else
        '0';

  ng <= '1' when signed(neg_out) < 0 else
        '0';

  outb <= neg_out;

end behavior;
