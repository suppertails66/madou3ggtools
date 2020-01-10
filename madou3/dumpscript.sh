set -o errexit

#mkdir -p script/orig
#make libsms && make madou2_scriptdmp
#./madou2_scriptdmp madou2.gg script/orig/

mkdir -p script/orig
make libsms && make madou3_scriptdmp
./madou3_scriptdmp madou3.gg script/orig/
