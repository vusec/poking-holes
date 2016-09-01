def fmtsz(i, params={})
  padding = ""
  padding = params[:padding] unless params[:padding].nil?
  decimal_points = 2
  decimal_points = params[:decimal_points] unless params[:decimal_points].nil?
  dk = ""
  if params[:binary_units]
    dk = "i"
  end

  [[2**30, "G#{dk}B"], [2**20, "M#{dk}B"], [2**10, "K#{dk}B"], [1, "B"]].each { |magn|
    mult = magn[0]
    unit = magn[1]
    if (i / mult) > 0 or ((0 == i) and (1 == mult))
      fmtstr = "%#{padding}.#{decimal_points}f"
      return "#{sprintf(fmtstr, i / mult.to_f)}#{unit}"
    end
  }
end
