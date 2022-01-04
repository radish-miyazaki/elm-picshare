port module WebSocket exposing (listen, receive)

-- 外を向いたポート（JavaScript側にデータを送り出す）


port listen : String -> Cmd msg



-- 内を向いたポート（JavaScript側からデータを受け取る）


port receive : (String -> msg) -> Sub msg
