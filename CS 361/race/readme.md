# README

## 1. Name and Drexel ID

**Name:** Utsav Chaudhary  
**Drexel ID:** uc49@drexel.edu

---

## 2. Instructions to Run the Code

To build and run the program, please follow the steps below:

1. **Compile the Code:**

    ```bash
    make
    ```

    This command will compile the source files and generate the executable.

2. **Run the Program:**

    ```bash
    ./bin/race [Number of Racers]
    ```

    Replace `[Number of Racers]` with the desired positive integer representing the number of racers in the simulation.

    **Example:**

    ```bash
    ./bin/race 5
    ```

    This will run the program with five racer threads.

3. **Clean Up Compiled Files:**

    ```bash
    make clean
    ```

    This command will remove the compiled object files and the executable.

4. **Generate Documentation (Optional):**

    ```bash
    make doc
    ```

    This will generate the Doxygen documentation for the code.

---

## 3. Short Essay Question 1: What Semaphores Did You Use and Why?

In this program, I did not use any semaphores. Instead, I utilized a thread-safe queue (`ConQueue<int>`) to manage synchronization between the game master and the racer threads. The `ConQueue` uses condition variables and mutexes to handle the producer-consumer relationship effectively. Semaphores were optional since the synchronization requirements were adequately met using these constructs. The condition variables allow threads to wait efficiently for resources without busy waiting, and mutexes ensure mutual exclusion where needed.

---

## 4. Short Essay Question 2: What Locks Did You Use and Why?

I used a mutex lock (`printLk`) to synchronize access to the standard output stream (`std::cout`). This lock ensures that when multiple threads attempt to print messages to the console, their outputs do not interleave and become unreadable. By wrapping the `std::cout` statements within a `std::lock_guard<std::mutex>,` I ensured that only one thread could write to the console simultaneously.

Additionally, the `ConQueue<int>` internally uses mutexes and condition variables to manage access to the shared queue of dice rolls. This prevents race conditions when multiple threads (the game master and racers) interact with the queue simultaneously.

---

## 5. Short Essay Question 3: Why Are You Confident the Program Can Never Deadlock?

I am confident that the program can never deadlock because of the careful design of the synchronization mechanisms:

- **Single Mutex Usage:** The only explicit mutex used (`printLk`) is confined to guarding print statements and is always locked and unlocked within the same scope using `std::lock_guard.` This prevents any possibility of forgetting to release the lock or holding it indefinitely.

- **No Circular Waits:** There are no scenarios where a thread holds one lock while waiting for another, eliminating the possibility of circular wait conditions—a key component of deadlocks.

- **Proper Use of Condition Variables:** The `ConQueue<int>` uses condition variables to manage the producer-consumer relationship without causing threads to block indefinitely while holding a lock.

- **Thread Termination Coordination:** The `completed` flags signal threads to terminate gracefully, ensuring all threads can complete their execution without waiting indefinitely for resources.

By avoiding nested locks and ensuring that locks are held only for the minimal necessary scope, the program eliminates the common causes of deadlocks.

---

## 6. Short Essay Question 4: Why Are You Confident the Program Can Never Let Any Thread Starve?

Thread starvation is prevented through the following design choices:

- **Fair Resource Distribution:** The game master continuously generates dice rolls and adds them to the shared queue, ensuring a steady supply of resources for the racer threads.

- **First-Come, First-Served Queue:** The `ConQueue<int>` operates on a first-in, first-out (FIFO) basis, which means that racer threads will receive dice rolls in the order they request them, preventing any single thread from being perpetually delayed.

- **Balanced Sleep Durations:** While racer threads sleep for random durations, these are uniformly distributed, preventing any thread from consistently experiencing longer delays than others.

- **No Priority Inversion:** All racer threads have equal priority and access to shared resources, and no priority mechanisms could cause lower-priority threads to starve.

I have minimized the risk of thread starvation by designing the program to treat all threads equally and ensuring that resources are distributed fairly.

---

## 7. Short Essay Question 5: What Was the Most Challenging Part of This Assignment?

The most challenging part of this assignment was ensuring proper synchronization between the multiple threads without introducing deadlocks or race conditions. Implementing the thread-safe queue (`ConQueue<int>`) required a deep understanding of condition variables and mutexes to handle the complexities of the producer-consumer problem efficiently. Coordinating the termination of threads using shared flags also required careful consideration to prevent premature exits or infinite loops. Balancing the randomness introduced by sleep durations and dice rolls while maintaining a fair and deadlock-free execution was a complex task that required careful coding and testing.

---