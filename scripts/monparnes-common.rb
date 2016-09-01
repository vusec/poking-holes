require 'shellwords'
require 'ruby-progressbar'

require 'fileutils'

class DummyProgressBar
  def increment
  end
end

class Runner
  def initialize(baseoutdir, total, progressbar, parallel)
    @start_time = Time.now
    @baseoutdir = baseoutdir

    @progressbar = DummyProgressBar.new
    if $stdout.isatty and $options[:progressbar]
      @progressbar = ProgressBar.create(:total => total, :smoothing => 0.1,
                                        :format => "%t: %w%i (%c/%C, ETA %f)")
    end
    @successes = 0
    @total = total
    @parallel = parallel
    if @parallel
      if $options[:progressbar]
        raise "Progressbar cannot be combined with parallel"
      end
      system("which parallel >/dev/null 2>&1")
      if $? != 0
        raise "GNU parallel is not in the path"
      end
    end
  end

  def doit(i, cmd, successfile, toremovefile)
    outdir = "#{@baseoutdir}/#{i}"
    FileUtils.mkdir_p(outdir)
    cmd = cmd.result(binding)
    successfile = "#{outdir}/#{successfile}"
    toremovefile = "#{outdir}/#{toremovefile}"
    cmd = "cmd=#{Shellwords.escape(cmd)}; eval \"$cmd\"; ret=$?; if test $ret -ne 0; then echo \"Error executing $cmd ($ret)\"; exit 7; else if test -f #{successfile}; then rm #{toremovefile}; fi; exit 0; fi"
    if @parallel
      puts(cmd)
    else
      system(cmd)
      ret = $?
      if ret != 0
        pute("Error executing '#{cmd}' (#{ret})")
        return
      end
    end
    @successes += 1
    @progressbar.increment
  end

  def done
    elapsed = sprintf("%.2f", Time.now - @start_time)
    if not @parallel
      puts("Successfully executed #{@successes}/#{@total} runs (#{@total - @successes} non-zero exits) [#{elapsed} seconds]")
    end
  end
end
