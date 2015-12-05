// Implementation of the routines required for simulating communication
// based on Posix threads.

#include "qpl_runtime.h"
#include "qpl_runtime_comm.h"

void error_exit (int code, string msg) {
    cerr << msg << " (code: " << code << ")" << endl;
    exit(1);
}

string float_to_string(float f) {
   std::ostringstream os;
   os << f ;
   return (os.str());
} 

string int_to_string(int i) {
   std::ostringstream os;
   os << i ;
   return (os.str());
} 

string long_to_string(long i) {
   std::ostringstream os;
   os << i ;
   return (os.str());
} 

string eval_datum (comm_t d) {
    string result;

    switch (d.dtype) { 
	case INT:
	    result.append("Integer, Value: ");
	    result.append(int_to_string(d.value.i));
	    break;
	case FLOAT:
	    result.append("Float, Value: ");
	    result.append(float_to_string(d.value.f));
	    break;
	case QBIT:
	    result.append("QBit, Location: ");
	    result.append(long_to_string((long)d.value.ptr));
	    break;
	case QINT:
	    result.append("QInt, Location: ");
	    result.append(long_to_string((long)d.value.ptr));
	    break;
	default:
	    result.append("Unknown type!");
	    exit(-1);
    }

    return result;
}

// WARNING: This is left with queue_lock held, so make sure to
// release it after the call! This function is not supposed to
// be called by the end user.
bool queue_has_data(queue<comm_t> *q, pthread_cond_t *queue_cond, 
		    pthread_mutex_t *cond_lock,
		    pthread_mutex_t *queue_lock) {
    // Wait until the queue has data in it. Returns with l held.
    while (1) {
	pthread_mutex_lock(queue_lock);
	if (!(q->empty())) return true;
	else {
	    pthread_mutex_unlock(queue_lock);
	    pthread_cond_wait(queue_cond, cond_lock);	    
	}
    }
}

struct comm_datum get_data (queue<comm_t> *data_queue, 
			    pthread_cond_t *queue_cond, 
			    pthread_mutex_t *cond_lock,
			    pthread_mutex_t *queue_lock) {
    comm_t ret;
    if (queue_has_data(data_queue, queue_cond, cond_lock, 
		       queue_lock)) {
	ret = data_queue->front(); data_queue->pop();
	pthread_mutex_unlock(queue_lock);
    }
    return ret;
}

void send_data (comm_t datum, queue<comm_t> *data_queue, 
		pthread_cond_t *queue_cond,
		pthread_mutex_t *cond_lock, 
		pthread_mutex_t *queue_lock) {
    pthread_mutex_lock(queue_lock);
    data_queue->push(datum);
    pthread_mutex_unlock(queue_lock);
    pthread_cond_broadcast(queue_cond);
}

// Frontend routine for the Posix thread library
void create_thread (pthread_t *tid, 
			 void * (*routine)(void *), string name) {
    int err_code;

    if (err_code = pthread_create(tid, NULL, routine, (void*)&name)) {
	cerr << "Runtime error: Could not create thread for " <<
	    name << "(error code: " << err_code << ")" << endl;
	exit (-1);
    }
}
