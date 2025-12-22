using Base.Threads

function compute_mandelbrot(width, height, max_iter)
    result = zeros(Int, height, width)
    y_chunks = collect(Iterators.partition(1:height, ceil(Int, height / nthreads())))
    
    @threads for chunk in y_chunks
        for y in chunk
            cy = (y - 1) / (height - 1) * 3.0 - 1.5
            for x in 1:width
                cx = (x - 1) / (width - 1) * 4.0 - 2.5
                zx = 0.0
                zy = 0.0
                iter = 0
                while iter < max_iter && zx*zx + zy*zy < 4.0
                    zx, zy = zx*zx - zy*zy + cx, 2*zx*zy + cy
                    iter += 1
                end
                result[y, x] = iter
            end
        end
    end
    return result
end

function save_ppm(filename, data, max_iter)
    height, width = size(data)
    open(filename, "w") do f
        write(f, "P6\n$width $height\n255\n")
        for y in 1:height
            for x in 1:width
                iter = data[y, x]
                r = UInt8(floor((iter / max_iter) * 255))
                g = UInt8(floor((iter / max_iter) * 255))
                b = UInt8(floor((iter / max_iter) * 200))
                write(f, r, g, b)
            end
        end
    end
end

function main()
    width = 800
    height = 600
    max_iter = 255
    
    println("Вычисление множества Мандельброта на $(nthreads()) потоках...")
    result = compute_mandelbrot(width, height, max_iter)
    
    save_ppm("mandelbrot.ppm", result, max_iter)
    println("Результат сохранен в mandelbrot.ppm")
    
    ascii_width = 120
    ascii_height = 40
    println("\nASCII-арт предпросмотр ($(ascii_width)x$(ascii_height)):")
    ascii_data = compute_mandelbrot(ascii_width, ascii_height, max_iter)
    chars = " .:-=+*#%@"
    for y in 1:ascii_height
        for x in 1:ascii_width
            idx = min(div(ascii_data[y, x] * length(chars), max_iter) + 1, length(chars))
            print(chars[idx])
        end
        println()
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
