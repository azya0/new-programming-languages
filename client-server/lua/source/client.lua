local socket = require("socket")

local config_file = "input/config.txt"
local input_file = "input/commands.txt"
local output_file = "output/responses.txt"

local function read_config()
    local f = io.open(config_file, "r")
    if not f then
        return nil, "Config file not found"
    end
    
    local device_id = f:read("*l")
    local key = f:read("*l")
    f:close()
    
    if not device_id or not key then
        return nil, "Invalid config format"
    end
    
    return device_id, key
end

local device_id, key = read_config()
if not device_id then
    print("Ошибка: " .. key)
    print("Создайте файл " .. config_file)
    os.exit(1)
end

local client = socket.connect("lua-server", 2000)
if not client then
    print("Ошибка подключения")
    os.exit(1)
end

client:send("AUTH|" .. device_id .. "|" .. key .. "\n")

client:settimeout(5)
local response = client:receive()
if not response or response ~= "AUTH_OK" then
    print("Ошибка авторизации")
    client:close()
    os.exit(1)
end

print("Устройство " .. device_id .. " авторизовано")

local function write_response(text)
    local f = io.open(output_file, "w")
    if f then
        f:write(text)
        f:close()
    end
end

write_response("READY")

while true do
    local f = io.open(input_file, "r")
    if f then
        local cmd = f:read("*a")
        f:close()
        
        if cmd and cmd ~= "" then
            cmd = cmd:gsub("\n", ""):gsub("\r", "")
            
            if cmd == "EXIT" then
                client:send("BYE\n")
                client:settimeout(2)
                local bye_response = client:receive()
                write_response(bye_response or "DISCONNECTED")
                break
            end
            
            client:send(cmd .. "\n")
            
            client:settimeout(5)
            local response = client:receive()
            
            if response then
                write_response(response)
            else
                write_response("TIMEOUT")
            end
        end
    end
    
    socket.sleep(0.5)
end

client:close()