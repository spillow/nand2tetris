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
  signal zxmod   : std_logic_vector(15 downto 0);
  signal zymod   : std_logic_vector(15 downto 0);
  signal nxmod   : std_logic_vector(15 downto 0);
  signal nymod   : std_logic_vector(15 downto 0);
  signal comp    : std_logic_vector(15 downto 0);
  signal neg_out : std_logic_vector(15 downto 0);
begin

  zxmod <= (others => '0') when zx = '1' else x;
  nxmod <= (not zxmod) when nx = '1' else zxmod;

  zymod <= (others => '0') when zy = '1' else y;
  nymod <= (not zymod) when ny = '1' else zymod;

  comp <= std_logic_vector(unsigned(nxmod) + unsigned(nymod)) when f = '1' else
          (nxmod and nymod);

  neg_out <= comp when no = '0' else
             not comp;

  zr <= '1' when neg_out = (neg_out'range => '0') else
        '0';

  ng <= '1' when signed(neg_out) < 0 else
        '0';

  outb <= neg_out;

end behavior;
