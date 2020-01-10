mkdir -p rsrc
mkdir -p rsrc/orig
mkdir -p rsrc_raw
mkdir -p rsrc_raw/grp
mkdir -p rsrc_raw/grp_dumped

make libsms && make rawdmp
make libsms && make grpdmp_gg
make libsms && make madou_decmp

./madou_decmp madou3.gg 0x34795 rsrc_raw/grp/title_bg1.bin
./madou_decmp madou3.gg 0x35A15 rsrc_raw/grp/title_bg2.bin
./madou_decmp madou3.gg 0x1E9EE rsrc_raw/grp/buttons_base.bin
./madou_decmp madou3.gg 0x1EA9E rsrc_raw/grp/buttons_title.bin
./madou_decmp madou3.gg 0x1EE4C rsrc_raw/grp/buttons_file.bin
#./madou_decmp madou3.gg 0x20000 rsrc_raw/grp/windows.bin
#./madou_decmp madou3.gg 0x2054F rsrc_raw/grp/dungeon1bg.bin
#./madou_decmp madou3.gg 0x2193D rsrc_raw/grp/dungeon1bg2.bin
./madou_decmp madou3.gg 0x229F8 rsrc_raw/grp/compass.bin
#./madou_decmp madou3.gg 0x1C020 rsrc_raw/grp/face1.bin
#./madou_decmp madou3.gg 0x1C1C4 rsrc_raw/grp/face2.bin
#./madou_decmp madou3.gg 0x22326 rsrc_raw/grp/carbuncle1.bin
./madou_decmp madou3.gg 0x1EB8F rsrc_raw/grp/buttons_map_save.bin
./madou_decmp madou3.gg 0x1EC75 rsrc_raw/grp/buttons_magic_item.bin
./madou_decmp madou3.gg 0x1ED7D rsrc_raw/grp/buttons_flee_lipemco.bin
./madou_decmp madou3.gg 0x1F000 rsrc_raw/grp/buttons_buy_sell_leave.bin
./madou_decmp madou3.gg 0x1EF1E rsrc_raw/grp/buttons_yes_no.bin
#./madou_decmp madou3.gg 0x1EF1E rsrc_raw/grp/test.bin

./madou_decmp madou3.gg 0x30000 rsrc_raw/grp/kero2_entrance.bin

for file in rsrc_raw/grp/*.bin; do
  FILEBASE=$(basename $file .bin)
  ./grpdmp_gg $file rsrc_raw/grp_dumped/${FILEBASE}.png -p "rsrc_raw/pal/main_bg_distinct.bin"
done

# this is garbage but gets copied to VRAM 3100 (text area)
# on game start anyway.
# i'm guessing this is a leftover from madou1, which used a static
# tile-based font rather than the dynamic loading from madou2 on.
#./grpdmp_gg madou3.gg rsrc_raw/test.png $((0x36DC+(0x20*10))) 0x18

./grpdmp_8bpp_gg madou3.gg rsrc_raw/font.png 0x1F515 0xA2

mkdir -p rsrc/orig
./tilemapdmp_gg madou3.gg 0x35b70 full 20 9 rsrc_raw/grp/title_bg1.bin 0 rsrc/orig/title_logo.png -p rsrc_raw/pal/title.bin
./tilemapdmp_gg madou3.gg 0x35CD8 full 20 2 rsrc_raw/grp/title_bg2.bin 0 rsrc/orig/title_logo.png -p rsrc_raw/pal/title.bin

# haha is this a voice sample?
#./madou_decmp madou3.gg 0x720A5 rsrc_raw/grp/test.bin

#./grpdmp_gg madou3.gg rsrc_raw/face3.png $((0x1C3F4+(0x20*10))) 0x16
#./grpdmp_gg madou3.gg rsrc_raw/face4.png $((0x1CA94+(0x20*10))) 0x16

# ./grpdmp_gg madou2.gg rsrc_raw/grp/button01.png $((0x1EA00+(0x20*10))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button02.png $((0x1EA00+(0x20*16))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button03.png $((0x1EA00+(0x20*22))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button04.png $((0x1EA00+(0x20*28))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button05.png $((0x1EA00+(0x20*34))) 6 -p rsrc_raw/main.pal
# 
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button06.png $((0x1EA00+(0x20*40))) 2 -p rsrc_raw/main.pal
# 
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button07.png $((0x1EA00+(0x20*42))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button08.png $((0x1EA00+(0x20*48))) 2 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button09.png $((0x1EA00+(0x20*50))) 2 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button10.png $((0x1EA00+(0x20*52))) 2 -p rsrc_raw/main.pal
# 
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button11.png $((0x1EA00+(0x20*54))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button12.png $((0x1EA00+(0x20*60))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button13.png $((0x1EA00+(0x20*66))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button14.png $((0x1EA00+(0x20*72))) 6 -p rsrc_raw/main.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/button15.png $((0x1EA00+(0x20*78))) 4 -p rsrc_raw/main.pal
# 
# ./grpdmp_gg madou2.gg rsrc_raw/grp/compass.png $((0x22020+(0x20*0))) 28 -p rsrc_raw/main_sprite_distinct.pal
# ./grpdmp_gg madou2.gg rsrc_raw/grp/buttons_title.png $((0x376E0+(0x20*0))) 12 -p rsrc_raw/main.pal
# 
# ./tilemapdmp_gg madou2.gg 0x37AED full 20 9 rsrc_raw/title_vram.bin 0 rsrc/orig/title_logo.png -p rsrc_raw/title.pal
# ./tilemapdmp_gg rsrc_raw/title_subtitle_map.bin 0x0 full 8 1 rsrc_raw/title_vram.bin 0 rsrc/orig/title_subtitle.png -p rsrc_raw/title.pal

