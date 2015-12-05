wget http://tph.tuwien.ac.at/~oemer/tgz/qcl-0.6.1.tgz
tar -xzf qcl-0.6.1.tgz
cd qcl-0.6.1/qc
make
cd ../../cqpl
sh gen.sh
g++ -c -I ../qcl-0.6.1/ -I . -I ../qcl-0.6.1/qc qpl_runtime.cc -o qpl_runtime.o
g++ -c -I ../qcl-0.6.1/ -I . -I ../qcl-0.6.1/qc qpl_runtime_comm.cc -o qpl_runtime_comm.o
echo 'Setup done'
