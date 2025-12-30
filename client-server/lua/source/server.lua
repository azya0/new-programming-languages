local socket = require("socket")

local devices = {
    ["thermo1"] = "key123",
    ["light1"] = "key456",
    ["lock1"] = "key789"
}

local device_status = {}

local server = socket.bind("0.0.0.0", 2000)
print("Сервер запущен на порту 2000")

while true do
    local client = server:accept()
    client:settimeout(10)
    
    local ip = client:getpeername()
    print("Новый клиент:", ip)
    
    local authorized = false
    local device_id = ""
    
    while true do
        local data, err = client:receive()
        
        if err then
            print("Ошибка:", err)
            break
        end
        
        print("Получено:", data)
        
        if not authorized then
            local cmd, id, key = data:match("^(%w+)|(%w+)|(%w+)$")
            
            if cmd == "AUTH" and devices[id] == key then
                authorized = true
                device_id = id
                client:send("AUTH_OK\n")
                print("Устройство авторизовано:", id)
            else
                client:send("AUTH_FAIL\n")
                break
            end
        else
            if data == "PING" then
                client:send("PONG\n")
            elseif data:match("^STATUS|") then
                local status = data:match("^STATUS|(.+)$")
                device_status[device_id] = status
                client:send("STATUS_OK\n")
                print("Статус сохранен:", device_id, status)
            elseif data == "GET_STATUS" then
                if device_status[device_id] then
                    client:send("STATUS|" .. device_status[device_id] .. "\n")
                else
                    client:send("NO_STATUS\n")
                end
            elseif data == "BYE" then
                client:send("GOODBYE\n")
                break
            else
                client:send("UNKNOWN_CMD\n")
            end
        end
    end
    
    client:close()
    print("Клиент отключен:", device_id or ip)
end