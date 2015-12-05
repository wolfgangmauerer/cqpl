/* This file is part of cqpl, a communication-capable quantum
   programming language.
   Copyright (C) 2005, Wolfgang Mauerer <wm@linux-kernel.net>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with cqpl.  If not, see <http://www.gnu.org/licenses/>. */

// Runtime support for qpl generated qcl backend binaries

#include<stdarg.h>
#include<sys/time.h>
#include<stdio.h>
#include "qpl_runtime.h"
#pragma implementation

#ifndef EPSILON_MATRIX
#define EPSILON_MATRIX 0.000001
#endif

extern void qc_srand(long seed);  // Save us from including qustates.h

// Initialisation of the runtime
void runtime_init() {
    // Initialise the random number generator with a random seed
    struct timeval *timer = new struct timeval;
    gettimeofday(timer, NULL);
    qc_srand((*timer).tv_sec);
}

// Helper functions
int int_log2 (int num) {
    return (int)(log((long double)num)/log((long double)2));
}

int int_sqrt (int num) {
    return (int)(sqrt((long double)num));
}

// Define some operators
opCNot *opcnot = new opCNot(1,1);
opBit *ophadamard = new opBit(1,1,1,-1,sqrt(0.5));
opNot *opnot = new opNot();

// The Fast fourier transform for n qubits
opVar opFFT(int n) {
  int i,j;
  opVar op;

  for(i=0; i<n; i++) {
      for(j=0; j < i; j++) {
	  op *= opX(n,n-i-1,n-j-1,i-j+1);
      }
      op *= opEmbedded(n,n-i-1,new opU2(PI/2,PI/2,PI/2,PI));
  }

  for(i=0; i<(n/2); i++) {
      op *= opSwap(n,1,i,n-i-1);
  }
  return op;
}

// Construct an operator that performs NOT operations on appropriate
// qubits in order to assign a value to a quantum variable. The
// resulting operator needs to be applied to a reset quantum state,
// i.e. one where all constituting qbits are set to zero.
// number denotes the number we want to set, whereas length specifies
// the # of qubits available.
opVar assignValue(word number, int length) {
    bitvec bv(length, number);
    opVar op;
    for (int count = 0; count < length; count++) {
	if (!(bv.getbits(count, 1).testzero())) {
	    // Bit at position count is set, so we need to apply
	    // a not gate
	    op *= opEmbedded(length, count, new opNot());
	}
	else {
	    // Bit not set, apply a unity gate
	    op *= opEmbedded(length, count, new opIdentity(1));
	}
    }

    return op;
}

/* Create a user defined matrix operator (adapted from Oemer's source)
   dimension specifies the total number of entries, eg. the dimension
   of a 4x4 matrix is 16 in this sense. */
opMatrix* userOperator(int dimension, complx* elements[]) {
    int i, j, n, k, dim;
    tComplex z;
    dim = int_sqrt(dimension); // The matrix is dim x dim
    n = int_log2(dim);  // dim = 2^{n}, where n is the # of qbits of a state
                        // the matrix operator can act on

    // Although the compiler should have already checked unitarity
    // of the arguments, we nethertheless do it once more here.
    // (rationale: useful for debugging the runtime)
    // Multiply the matrix with it's conjugate transpose
    // and check if the result is a unit matrix up to numeric
    // inaccuracies
    for(i = 0; i < dim; i++) {
	for(j = 0; j < dim; j++) {
	    z = 0;
	    for(k = 0; k < dim; k++) {
		z += *elements[i*dim + k] *
		     conj(*elements[j*dim + k]);
	    }

	    // Elements on the diagonal are supposed to be 1,
	    // whereas all off-diagonal elements must be 0.
	    if(i == j) {
		z -= 1;
	    }

	    if(abs(z) > EPSILON_MATRIX) {
		// This should have already been caught by the compiler
		cerr << "Internal error: Matrix is not unitary!" << endl;
		exit(-1);
	    }
	}
    }

    // Everything fine, so construct the desired operator now
    term* t[dim];
    for(i = 0; i < dim; i++) {
	k = 0;
	t[i]=new term[dim + 1];
	for(j = 0; j < dim; j++) {
	    z = *elements[j*dim + i];
	    if(z == tComplex(0)) continue;
	    t[i][k++] = term(bitvec(n,j),z);
	}
    }

    opMatrix *op = new opMatrix(n, &t[0]);
    return op;
}


// Dump the spectrum of a given quantum state (adapted from Oemer)
string regstr(const bitvec& v,int width=0) {
    int i,c,z=0;
    char s[v.length() + 3];
    char *p=s;

    for(i = v.length()-1; i >= 0; i--) {
	z|=v[i];
	if(width && !z && i >= width) continue;
	*(p++)=(v[i] ? '1' : '0');
    }
    *(p++)=0;

    return string(s);
}

string probstr(double r) {
    int optDumpPrecision=5;
    char s[2*optDumpPrecision+80];
    sprintf(s,"%.*g",optDumpPrecision,r);
    return s;
}

void dump_quantum_value(quState *qs,ostream *o = &cout) {
    double optDumpEpsilon = 0.00001;
    spectrum_iter i;
    state_iter s;
    int width = 0;
    ostream *f= o ? o : &cout;
    spectrum_map *pm=qs->new_spectrum_map(optDumpEpsilon);

    for(i = pm->begin(); i != pm->end(); i++) {
	if(i != pm->begin()) (*f) << ", ";
	(*f) << probstr((*i).second);
	(*f) << " ";
	(*f) << "|" + regstr((*i).first,width) + ">";
	}

    delete pm;
    (*f) << endl;
}
