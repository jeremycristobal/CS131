import sys
import asyncio
import time
import json
import aiohttp

key = 'my_key'

server_labels = {
    "Hill": 12040,
    "Jaquez": 12041,
    "Smith": 12042,
    "Campbell": 12043,
    "Singleton": 12044
}

server_communicates = {
    "Hill": ["Jaquez", "Smith"],
    "Jaquez": ["Hill", "Singleton"],
    "Smith": ["Hill", "Campbell", "Singleton"],
    "Campbell": ["Smith", "Singleton"],
    "Singleton": ["Jaquez", "Smith", "Campbell"]
}

clients = {}

def is_num(str):
    try:
        float(str)
        return True
    except ValueError:
        return False

def get_coords(coords):
    if coords[0] != '+' and coords[0] != '-':
        return None
    
    lat = coords[0]
    k = 1
    onedec = True
    while k < len(coords):
        if coords[k] == '+' or coords[k] == '-':
            break
        if coords[k] == '.' and onedec:
            onedec = False
        elif not coords[k].isdigit():
            return None
        lat += coords[k]
        k += 1

    if k >= len(coords) - 1:
        return None

    onedec = True
    lng = coords[k]
    k+= 1
    while k < len(coords):
        if coords[k] == '.' and onedec:
            onedec = False
        elif not coords[k].isdigit():
            return None
        lng += coords[k]
        k += 1

    return (lat, lng)

def check_messages(messages):
    if len(messages) < 1:
        return None

    if len(messages) == 4:
        if messages[0] == 'IAMAT':
            coordinates = get_coords(messages[2])
            if coordinates:
                return 'IAMAT'
        elif messages[0] == 'WHATSAT':
            if is_num(messages[2]) and is_num(messages[3]):
                rad = int(messages[2])
                inf = int(messages[3])
                if rad > 0 and rad <= 50 and inf > 0 and inf <= 20:
                    return 'WHATSAT'
    elif len(messages) == 6:
        if messages[0] == 'AT':
            return 'AT'

    return None

async def flood(message):
    for k in server_communicates[server_name]:
        try:
            reader, writer = await asyncio.open_connection('127.0.0.1', server_labels[k])
            log_file.write('Sending from ' + server_name + ' to ' + k + '\n')
            writer.write(message.encode())
            await writer.drain()
            log_file.write('Closing connection to ' + k + '\n')
            writer.close()
            await writer.wait_closed()
        except:
            log_file.write('Error communicating to ' + k + '\n')
            pass

async def get_places(url, inf):
    log_file.write('Attempting to connect to ' + url + '\n')
    async with aiohttp.ClientSession() as session:
        result = await fetch(session, url)
        places = json.loads(result)
        log_file.write('Successfully retreived places\n')

        if len(places['results']) <= inf:
            return places
        else:
            places['results'] = places['results'][:inf]
            return json.dumps(places, indent=4)

async def fetch(session, url):
    async with session.get(url) as response:
        return await response.text()

async def handle_connection(reader, writer):
    while not reader.at_eof():
        data = await reader.readline()
        message = data.decode()
        received = time.time()

        messages = message.split()
        message_type = check_messages(messages)
    
        respond = ''
        log_file.write('Received: ' + message)

        if message_type == 'IAMAT':
            name = messages[1]
            time_diff = str(received - float(messages[3]))
            clients[name] = [messages[2], messages[3], time_diff, server_name]
            if time_diff[0] != '-' and time_diff != '0':
                time_diff = '+' + time_diff
                respond = 'AT %s %s %s %s %s' % (server_name, time_diff, name, messages[2], messages[3])
                asyncio.ensure_future(flood(respond))
                
        elif message_type == 'WHATSAT':
            name = messages[1]
            if name not in clients:
                respond = '? ' + message
            rad = str(int(messages[2]) * 1000)
            inf = int(messages[3])
            old_coords, old_sent, old_diff, old_server = clients[name]
            lat, lng = get_coords(old_coords)
            good_coords = lat.strip('+') + ',' + lng.strip('+')
            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s&radius=%s&key=%s' % (good_coords, rad, key)        
            places = await get_places(url, inf)
            res_fmt = 'AT %s %s %s %s %s\n' % (server_name, old_diff, name, old_coords, old_sent)
            respond = res_fmt + str(places).rstrip('\n') + '\n\n'

        elif message_type == 'AT':
            log_file.write('Received flood\n')
            name = messages[3]
            if name in clients:
                old_sent = (float((clients[name])[1]))
                if float(messages[5]) > old_sent:
                    log_file.write('Updating data for client ' + name + '\n')
                    clients[name] = [messages[4], messages[5], messages[2], messages[1]]
                    await flood(message)
                else:
                    log_file.write('Already received data.  Stop flood\n')
                    pass
            else:
                log_file.write('Data for new client ' + name + '\n')
                clients[name] = [messages[4], messages[5], messages[2], messages[1]]
                await flood(message)

        else:
            respond = '? ' + message

        if message_type != 'AT':
            log_file.write("Returning '" + respond + "' to client\n")
            writer.write(respond.encode())
            await writer.drain()
        
    writer.close()

def main():
    if len(sys.argv) != 2:
        print('Invalid number of arguments.')
        exit()
    elif sys.argv[1] not in server_labels:
        print('Invalid server name.')
        exit()
    
    global server_name
    global log_file
    global event_loop

    server_name = sys.argv[1]
    
    log_name = server_name + 'log.txt'
    log_file = open(log_name, 'w+')
    
    log_file.write('Starting up ' + server_name + ' server\n')

    event_loop = asyncio.get_event_loop()
    coroutine = asyncio.start_server(handle_connection, host='127.0.0.1', port=server_labels[server_name], loop = event_loop)
    server = event_loop.run_until_complete(coroutine)

    try:
        event_loop.run_forever()
    except KeyboardInterrupt:
        pass

    log_file.write('Shutting down server\n')

    server.close()
    event_loop.run_until_complete(server.wait_closed())
    event_loop.close()
    log_file.close()

if __name__ == '__main__':
    main()
