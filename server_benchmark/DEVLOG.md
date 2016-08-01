
[2016.07.24] {First TechEmpower / snap run}
----------------------------------------

I ran:

    toolset/run-tests.py --install server --mode verify --test snap

It completed successfully and said:

    Results are saved in results/20160723204852

But then srun timed out and "vagrant ssh" was killed.  Now after I
vagrant up again... that directory is missing!  Did force shutting
down the virtual machine lose the data?

Anyway, `--mode verify` is probably the wrong mode.  Trying this:

    time toolset/run-tests.py --mode benchmark --test snap


[2016.07.25] {Grepping the snap packages}

    167 references to IORef in the 3 snap packages.
    50 readIORefs.  13 atomicModify.  28 writeIORef.  1 non-atomic modifyIORef.
    40 newIORef

    (+ 50 13 28 1 40) = 132

Remaining references are import lines or the types.

Here are all the writes:

    snap-core-0.9.8.0/src/Snap/Internal/Test/RequestBuilder.hs:128:              liftIO $ writeIORef (rqBody rq') (SomeEnumerator enumEOF)
    snap-core-0.9.8.0/src/Snap/Internal/Test/RequestBuilder.hs:200:    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator enumEOF
    snap-core-0.9.8.0/src/Snap/Internal/Test/RequestBuilder.hs:207:    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator enumEOF
    snap-core-0.9.8.0/src/Snap/Internal/Test/RequestBuilder.hs:214:    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator $ enumBS b
    snap-core-0.9.8.0/src/Snap/Internal/Test/RequestBuilder.hs:225:    liftIO $ writeIORef (rqBody rq) $ SomeEnumerator $ enumBS b
    snap-core-0.9.8.0/src/Snap/Internal/Test/RequestBuilder.hs:371:    liftIO $ writeIORef (rqBody rq0) $ SomeEnumerator $ enumBS b
    snap-core-0.9.8.0/src/Snap/Internal/Types.hs:319:                    writeIORef (rqBody req) $
    snap-core-0.9.8.0/src/Snap/Internal/Types.hs:329:              liftIO $ writeIORef (rqBody req) en
    snap-core-0.9.8.0/src/Snap/Internal/Types.hs:386:    liftIO $ writeIORef ioref (SomeEnumerator enumEOF)
    snap-core-0.9.8.0/src/Snap/Util/FileUploads.hs:899:    writeIORef stateRef emptyUploadedFilesState
    snap-core-0.9.8.0/src/Snap/Util/FileUploads.hs:928:    writeIORef stateRef $ state { _currentFile = Just fph }
    snap-core-0.9.8.0/src/Snap/Util/FileUploads.hs:945:               writeIORef stateRef $
    snap-core-0.9.8.0/test/suite/Snap/Core/Tests.hs:328:                          (\z -> liftIO $ writeIORef ref $! z+1)
    snap-core-0.9.8.0/test/suite/Snap/Core/Tests.hs:332:                           (\z -> liftIO $ writeIORef ref $! z+1)
    snap-core-0.9.8.0/test/suite/Snap/Util/FileUploads/Tests.hs:424:    writeIORef (rqBody rq) $ SomeEnumerator slowEnum
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server/TimeoutManager.hs:151:        writeIORef inactivity False
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server/TimeoutManager.hs:187:    writeIORef stateRef state'
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server/TimeoutManager.hs:198:cancel h = writeIORef (_state h) Canceled
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server/TimeoutManager.hs:221:            writeIORef inactivity True
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server.hs:560:              liftIO $ writeIORef (rqBody req)
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server.hs:573:            liftIO $ writeIORef (rqBody req) (SomeEnumerator e)
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server.hs:596:            liftIO $ writeIORef (rqBody rq) enum
    snap-server-0.9.5.1/src/Snap/Internal/Http/Server.hs:640:            liftIO $ writeIORef (rqBody req) $ SomeEnumerator e'
    snap-server-0.9.5.1/src/System/FastLogger.hs:232:            mask_ $ openIt >>= writeIORef href
    snap-server-0.9.5.1/src/System/FastLogger.hs:233:            writeIORef lastOpened t
    snap-server-0.9.5.1/test/suite/Snap/Internal/Http/Server/Tests.hs:575:    liftIO $ writeIORef (rqBody req) (SomeEnumerator $ joinI . I.take 0)
    snap-server-0.9.5.1/test/suite/Snap/Internal/Http/Server/TimeoutManager/Tests.hs:48:    h <- TM.register (writeIORef ref 1) mgr
    snap-server-0.9.5.1/test/suite/Snap/Internal/Http/Server/TimeoutManager/Tests.hs:66:    h <- TM.register (writeIORef ref 1) mgr


[2016.08.01] {Grabbing SC results}
----------------------------------

It's running now, with results going here:

    /u/crest-team/results_backup/TechEmpower/sc-v0.4/c-swarm_1470024513//

And here: c-swarm_1470042361

I was running these manually.  So I did something delicate and issued
a sleep cemmand then the second batch (c-swarm_1470042361).  But the
first command took longer than I expected, finishing at 4:26am.

The first measurement of the second command came in at 5:13am.  The
question is how long it took it to build the VM and start the second batch.

I issued my sleep for 220 minutes at around 1:30am before going to
sleep.  Thus it should have run waited until 5:10 to run.  This means
that it cleared the 4:26 finish time, and the results should not
interfere with eachother.

Ugh it's at 9 hrs now and I think it was a 12 hour reservation.  3
hours to finish.
