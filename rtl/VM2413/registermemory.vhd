library ieee;
    use ieee.std_logic_1164.all;
    use ieee.std_logic_unsigned.all;

entity RegisterMemory is
    port (
        clk     : in    std_logic;
        reset   : in    std_logic;
        addr    : in    std_logic_vector(3 downto 0);
        wr      : in    std_logic;
        idata   : in    std_logic_vector(23 downto 0);
        odata   : out   std_logic_vector(23 downto 0);
        -- savestate sideband
        ss_wren : in    std_logic := '0';
        ss_waddr: in    std_logic_vector(3 downto 0) := (others => '0');
        ss_wdata: in    std_logic_vector(23 downto 0) := (others => '0');
        ss_dump : out   std_logic_vector(215 downto 0) := (others => '0')
    );
end RegisterMemory;

architecture rtl of registermemory is
    type regs_array_type is array (0 to 8) of std_logic_vector(23 downto 0);
    signal regs_array : regs_array_type;
    attribute ramstyle : string;
    attribute ramstyle of regs_array : signal is "logic";

begin
    process( reset, clk )
        variable init_state : integer range 0 to 9;
    begin
        if reset = '1' then
            init_state := 0;
        elsif rising_edge(clk) then
            if init_state /= 9 then
                regs_array( init_state ) <= (others => '0');
                init_state := init_state + 1;
            elsif ss_wren = '1' then
                regs_array( conv_integer(ss_waddr) ) <= ss_wdata;
            elsif wr = '1' then
                regs_array( conv_integer(addr) ) <= idata;
            end if;
            odata <= regs_array( conv_integer(addr) );
        end if;
    end process;

    dump_gen : for i in 0 to 8 generate
        ss_dump(i*24+23 downto i*24) <= regs_array(i);
    end generate;
end rtl;
