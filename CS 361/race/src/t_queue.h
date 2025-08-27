/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @date 10/25/24
 @section Description
 
 Concurrent safe wrapper for std::queue
 */

#ifndef _QUEUE_H_
#define _QUEUE_H_
#include <memory>
#include <stdexcept>
#include <mutex>
#include <queue>
#include <condition_variable>

/**
 A concurrent safe queue.
 */
template <typename T>
class ConQueue{
private:
    std::queue<T> q;/**< The classic queue to protect.*/
    mutable std::mutex lock;/**< Lock for safety.*/
    std::condition_variable cv;/**< Condition Variable to notify new items.*/
public:
    /**
     Add a new item to the queue
     @param v is the item to add.
     */
    void push(T v);
    /**
     Pop a value when one appears.
     @return the value on the front of the queue when available
     */
    T waitToPop();
};

//Add under lock and notify waiting thread
template <typename T>
void ConQueue<T>::push(T v){
    std::lock_guard<std::mutex> lk(lock);
    q.push(v);
    cv.notify_one();
}

//Wait for a value before removing it
template <typename T>
T ConQueue<T>::waitToPop(){
    T item;
    {
        std::unique_lock<std::mutex> lk(lock);
        cv.wait(lk, [this]{ return !q.empty(); });
        item = std::move(q.front());
        q.pop();
    }
    return item;
}

#endif

