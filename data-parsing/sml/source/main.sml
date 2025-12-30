structure LogParser =
struct
  type logEntry = string * string * string * string * string * string * string

  fun parseLine line =
    let
      fun scan nil = (nil, nil)
        | scan (#" " :: rest) = (nil, rest)
        | scan (c :: rest) =
            let val (token, remaining) = scan rest
            in (c :: token, remaining) end

      fun parseHost chars =
        let val (host, rest) = scan chars
        in (implode host, rest) end

      fun parseIdent chars =
        let val (ident, rest) = scan chars
        in (implode ident, rest) end

      fun parseAuthuser chars =
        let val (auth, rest) = scan chars
        in (implode auth, rest) end

      fun parseDate chars =
        let
          fun take n nil = (nil, nil)
            | take 0 rest = (nil, rest)
            | take n (c :: rest) =
                let val (taken, remaining) = take (n-1) rest
                in (c :: taken, remaining) end
          val (date, rest1) = take 1 chars
          val (datePart, rest2) = take 26 (tl rest1)
        in (implode (date @ datePart), rest2) end

      fun parseRequest chars =
        let
          fun takeUntil quote nil = (nil, nil)
            | takeUntil quote (c :: rest) =
                if c = quote then (nil, rest)
                else
                  let val (taken, remaining) = takeUntil quote rest
                  in (c :: taken, remaining) end
          val (request, rest) = takeUntil #"\"" (tl chars)
        in (implode request, tl rest) end

      fun parseStatus chars =
        let val (status, rest) = scan chars
        in (implode status, rest) end

      fun parseBytes chars =
        (implode chars, nil)

      val chars = explode line
      val (host, rest1) = parseHost chars
      val (ident, rest2) = parseIdent (tl rest1)
      val (authuser, rest3) = parseAuthuser (tl rest2)
      val (date, rest4) = parseDate (tl rest3)
      val (request, rest5) = parseRequest (tl rest4)
      val (status, rest6) = parseStatus (tl rest5)
      val (bytes, _) = parseBytes (tl rest6)
    in
      (host, ident, authuser, date, request, status, bytes)
    end

  fun getStatus (_, _, _, _, _, status, _) = status
  fun getRequest (_, _, _, _, request, _, _) = request

  fun extractResource request =
    let
      val words = String.tokens (fn c => c = #" ") request
    in
      if List.length words >= 2
      then List.nth(words, 1)
      else ""
    end
end

structure LogAnalysis =
struct
  fun readLogFile filename =
    let
      val ins = TextIO.openIn filename
      fun readLines acc =
        case TextIO.inputLine ins of
            SOME line => readLines (line :: acc)
          | NONE => List.rev acc
      val lines = readLines []
      val _ = TextIO.closeIn ins
    in
      lines
    end

  fun processLogs filename =
    let
      val lines = readLogFile filename
      val entries = List.map LogParser.parseLine lines

      fun countStatuses entries =
        let
          fun addStatus (status, counts) =
            case List.find (fn (s, _) => s = status) counts of
                SOME (s, c) => (status, c+1) :: List.filter (fn (s', _) => s' <> status) counts
              | NONE => (status, 1) :: counts
        in
          List.foldl (fn (entry, acc) => addStatus (LogParser.getStatus entry, acc)) [] entries
        end

      fun countResources entries =
        let
          fun addResource (resource, counts) =
            case List.find (fn (r, _) => r = resource) counts of
                SOME (r, c) => (resource, c+1) :: List.filter (fn (r', _) => r' <> resource) counts
              | NONE => (resource, 1) :: counts
          val resources = List.map (fn entry => LogParser.extractResource (LogParser.getRequest entry)) entries
        in
          List.foldl (fn (res, acc) => addResource (res, acc)) [] resources
        end

      fun takeTopN n list =
        let
          fun compare ((_, a), (_, b)) = a < b
          val sorted = ListMergeSort.sort compare list
        in
          List.take (List.rev sorted, Int.min(n, List.length sorted))
        end

      val statusCounts = countStatuses entries
      val resourceCounts = countResources entries
      val topResources = takeTopN 5 resourceCounts
    in
      (statusCounts, topResources)
    end

  fun printResults (statusCounts, topResources) =
    let
      fun printStatusCounts nil = ()
        | printStatusCounts ((status, count)::rest) =
            (print ("Status " ^ status ^ ": " ^ Int.toString count ^ "\n");
             printStatusCounts rest)
      fun printTopResources nil = ()
        | printTopResources ((resource, count)::rest) =
            (print ("Resource " ^ resource ^ ": " ^ Int.toString count ^ "\n");
             printTopResources rest)
    in
      (print "HTTP Status Statistics:\n";
       printStatusCounts statusCounts;
       print "\nTop 5 Resources:\n";
       printTopResources topResources)
    end
end

val _ = LogAnalysis.printResults (LogAnalysis.processLogs "input/sample.log")