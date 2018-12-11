module TestHelpers

    let waitForDebugger =
        if not(System.Diagnostics.Debugger.IsAttached) then
          printfn "Please attach a debugger, PID: %d" (System.Diagnostics.Process.GetCurrentProcess().Id)
        while not(System.Diagnostics.Debugger.IsAttached) do
          System.Threading.Thread.Sleep(100)
        System.Diagnostics.Debugger.Break()