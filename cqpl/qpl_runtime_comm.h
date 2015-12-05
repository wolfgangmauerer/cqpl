// Runtime support for the communcation implementation

#ifndef QPL_RUNTIME_COMM_H
#define QPL_RUNTIME_COMM__H

#include <pthread.h>
#include <string.h>	
#include <stdlib.h>
#include <iostream>
#include <queue>
#include <sstream>
#include <string>

using namespace std;

// A datum which can be sent over a communication channel
enum datum_type { INT, FLOAT, BIT, QBIT, QINT };

struct comm_datum {
    union {
	int i;
	float f;
	bit b;
	void* ptr;
    } value;
    datum_type dtype;
};

typedef struct comm_datum comm_t;

// Prototypes for some helper routines
string long_to_string(long i);
string int_to_string(int i);
string float_to_string(float f);
void error_exit (int code, string msg);

// and the prototypes for the functions which implement communication
bool queue_has_data(queue<comm_t> *q, pthread_cond_t *queue_cond, 
		    pthread_mutex_t *cond_lock,
		    pthread_mutex_t *queue_lock);
struct comm_datum get_data (queue<comm_t> *data_queue, 
			    pthread_cond_t *queue_cond, 
			    pthread_mutex_t *cond_lock,
			    pthread_mutex_t *queue_lock);
void send_data (comm_t datum, queue<comm_t> *data_queue, 
		pthread_cond_t *queue_cond,
		pthread_mutex_t *cond_lock, 
		pthread_mutex_t *queue_lock);
void create_thread (pthread_t *tid, void * (*routine)(void *), string name);

// Finally, include the template based routines (since this concerns
// quantum memory management, we need to do it here because we need
// some locking in there
#include "qpl_runtime_templates.h"
#endif
