const WebSocket = require('ws')

const wss = new WebSocket.Server({ port: 8000 })

wss.on('connection', ws => {
    ws.send(JSON.stringify({ name: "Jonathan Linnestad", company: "Anleggsmannen" }))
})
