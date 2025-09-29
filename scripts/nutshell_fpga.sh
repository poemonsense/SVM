set -xe

cd $1
export NOOP_HOME=$1
make BOARD=PXIe
cd build/rtl
sed -i "s|module NutShell(|module NutShellBase(|g" NutShell.sv
sed -i "s|NutShell nutshell|NutShellBase nutshell|g" NutShellTop.sv
sed -i "s|module Top(|module NutShell(|g" TopMain.sv
mv TopMain.sv TopMain.v
