// Runtime helpers for the qcl backend of qpl

#ifndef QPL_RUNTIME_H
#define QPL_RUNTIME_H

#include <string>
#include <map>
#include <complex>
#include "qustates.h"
#include "types.h"
#include "operator.h"

using namespace std;

class qplCallContext {
 public:
    qplCallContext() {
	context = new map<string, quState*>();
    }

    quState* get(string val) {
	return (*context)[val];
    }

    void set(string val, quState *state) {
	(*context)[val] = state;
    }

    void reset() {
	if (context != NULL) {
	    delete context;
	    context = new map<string, quState*>;
	}
    }

 private:
    map<string, quState*> *context;
};


typedef bool bit;
typedef union {
    int i;
    float f;
    bit b;
} class_val_t;

class qplReturnContext {
 public:
    int get_int(string val) {
	return context[val].i;
    }

    bit get_bit(string val) {
	return context[val].b;
    }

    float get_float(string val) {
	return context[val].f;
    }

    void set_int(string val, int value) {
	context[val].i = value;
    }

    void set_bit(string val, bit value) {
	context[val].b = value;
    }

    void set_float(string val, float value) {
	context[val].f = value;
    }

 private:
    map<string, class_val_t> context;
};


// A quantum Integer is nothing else than a quWord with the proper
// number of bits (namely sizeof(int) == 32)
class quInt : public quSubString {
public:
  quInt(int offs,quState& base);
  int get();
  void set(int v);
};

inline quInt::quInt(int offs,quState& base)
  : quSubString(32,offs,base) {}

inline void quInt::set(int v) {
    reduce(bitvec(mapbits(),v));
}

inline int quInt::get() {
    return measure().getword(0,mapbits());
}


// Instantiate often-needed operators
// Predefined operators (Note that CPhase needs to be instatiated
// every time it is called because the argument changes)
extern opCNot *opcnot;
extern opBit *ophadamard;
extern opNot *opnot;
extern unsigned long qMemPos;
extern quBaseState local_mem;
opVar opFFT(int n);
opVar assignValue(word number, int length);

// Some more support stuff we need for random things
template<class T> T** create_array(int length, ...);
template<class T> T* allocateMem(int length);
opMatrix* userOperator(int dimension, complx* elements[]);
void dump_quantum_value(quState *qs, ostream *o);
void runtime_init();

static int bits_per_int = sizeof(int);

#endif
