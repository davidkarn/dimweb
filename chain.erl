-module(chain).
-compile(export_all).

-record(block,          {hash,
			 previous_hash
			 data,
			 index,
			 nonce,
			 timestamp}).

-record(wallet,         {private_key,
		         public_key}).

-record(transaction,    {id,
			 sender,
			 recipient,
			 value,
			 signature,
			 inputs,
			 outputs,
			 sequence}).

-record(message,        {sender,
			 type=message
			 receiver,
			 message,
			 attachment,
			 reference}).
		  
-record(follow,         {sender,
			 type=follow,
			 receiver,
			 message,
			 attachment,
			 reference}).

-record(registration,   {public_key,
			 type=registration,
			 name,
			 avatar,
			 bio,
			 location,
			 additional_data}).

-record(header,         {last_index}).
			 

-define(BUCKET,                <<"blocks">>).
-define(MESSAGES_BLOCK_NAME,   <<"pending_messages">>).
-define(HEADER_BLOCK_NAME,     <<"header_messages">>).

get_db() ->
    {ok, Db} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    Db.

json_to_message(JSON) ->
    Object   = element(1, JSON),
    Type     = proplists:lookup(<<"type">>, Object),
    json_object_to_message(Type, Object).

value(Key, Object) ->
     proplists:lookup(Key, Object).

json_object_to_message(<<"message">>, Object) ->
    #message{sender      = value(<<"sender">>,       Object),
	     type        = message,
	     receiver    = value(<<"receiver">>,     Object),
	     message     = value(<<"message">>,      Object),
	     attachment  = value(<<"attachment">>,   Object),
	     reference   = value(<<"reference">>,    Object)};

json_object_to_message(<<"follow">>, Object) ->
    #message{sender      = value(<<"sender">>,       Object),
	     type        = follow,
	     receiver    = value(<<"receiver">>,     Object),
	     message     = value(<<"message">>,      Object),
	     attachment  = value(<<"attachment">>,   Object),
	     reference   = value(<<"reference">>,    Object)};

json_object_to_message(<<"registration">>, Object) ->
    #message{public_key        = value(<<"public_key">>,       Object),
	     type              = registration,
	     name              = value(<<"name">>,             Object),
	     avatar            = value(<<"avatar">>,           Object),
	     bio               = value(<<"bio">>,              Object),
	     additional_data   = value(<<"additional_data">>,  Object),
	     location          = value(<<"location">>,         Object)}.

store_message(Message, Db) ->
    Messages = get_messages_block(Db),
    store_messages_block([Message|Messages], Db),
    if 
	length(Messages) >= 10 ->
	    append_block()
    end.

append_block(Db) ->
    Messages        = get_messages_block(Db),
    Header          = get_header_block(Db),
    Previous        = get_last_block(Db),
    Block           = make_block(Previous_block
				 Messages),
    UpdatedHeader   = Header#header{last_index=Block#block.index}

    store_messages_block([]),
    put_block(Db,
	      Block#block.index,
	      Block),

    spawn(fun() ->
		  mine(Block, Db) end).

put_block(Db, Index, Object) ->
    riakc_pb_socket:put(
      Db,
      riakc_obj:new(
	[?BUCKET],
	Index,
	term_to_binary(Block))).

get_latest_messages(Take) ->
    get_latest_messages(Take, 1).

get_latest_messages(Take, AfterBlockIndex) ->
    Header      = get_header_block(),
    LastIndex   = Header#header.last_index,
    lists:reverse(
      get_latest_messages(Take, AfterBlockIndex, LastIndex)).

get_latest_messages(Take, AfterBlockIndex, 0) -> [];
get_latest_messages(Take, AfterBlockIndex, LastIndex) ->
    if 
	Take < 0 ->
	    [];
	AfterBlockIndex <= LastIndex ->
	    [];
	true ->
	    Block    = get_block(LastIndex),
	    Messages = Block#block.data,
	    lists:append(lists:reverse(Messages),
			 get_latest_messages(Take - length(Messages)))
    end.
    
get_block(Db, Index) ->
    {Status, Value} = riakc_pb_socket:get(
			Db,
			[?BUCKET],
			Index),

    if Value == notfound ->
	    {Status, Value};
       true ->
	    {Status, binary_to_term(Value)}
    end.
    
get_header_block(Db) ->
    {Status, Value} = get_block(Db, 
				[?HEADER_BLOCK_NAME]),
    Value.

get_messages_block(Db) ->
    {Status, Value} = get_block(Db,
				[?MESSAGES_BLOCK_NAME]),
    if 
	Value == notfound ->
	    [];
	true ->
	    lists:reverse(Value)
    end.

get_header_block(Db) ->
    {Status, Value} = get_block(Db,
				[?HEADER_BLOCK_NAME]),
    if 
	Value == notfound ->
	    Header = #header{last_index=1},
	    put_block(Db,
		      [?HEADER_BLOCK_NAME],
		      Header),
	    Header;
		
	true ->
	    Value
    end.
 
store_messages_block(Messages, Db) ->
    put_block(Db,
	      [?MESSAGES_BLOCK_NAME],
	      Messages).

mine(Block, Db) ->
    MinedBlock = mine_block(Block),
    put_block(Db,
	      MinedBlock#block.index,
	      MinedBlock).

make_wallet() ->
    {PublicKey, PrivateKey} = crypto:generate_key(ecdh, secp128r1),
    #wallet(private_key:    PrivateKey,
	    public_key:     PublicKey}.
	       
wallet_sign(Wallet, Data) ->
    crypto:sign(ecdsa, sha256, Data, [Wallet#wallet.private_key, secp128r1]).

wallet_verify(Wallet, Data, Signature) ->
    crypto:verify(ecdsa, sha256, Data, Signature, [Wallet#wallet.public_key, secp128r1]).

make_block(Previous_Block, Data) ->
    Block = #block{previous_hash   = Previous_Block#block.hash,
		   index           = Previous_Block#block.index + 1,
		   data            = Data,
		   timestamp       = os:timestamp()},
    Block#block{hash = hash_block(Block)}.

hash_block(Block) ->
    crypto:hash(sha256, 
		term_to_binary({Block#block.previous_hash,
				Block#block.data,
				Block#block.index,
				Block#block.nonce,
				Block#block.timestamp})).

hash_block(Previous_Hash, Data, Timestamp, Index, Nonce) ->
    crypto:hash(sha256, 
		term_to_binary({Previous_Hash,
				Data,
				Index,
				Nonce,
				Timestamp})).

genesis_block() ->
    Block = #block{data            = 0,
		   index           = 0,
		   previous_hash   = 0,
		   timestamp       = os:timestamp()},
    Block#block{hash = hash_block(Block)}.

make_chain() ->
    [genesis_block()].

add_to_chain(Chain, Block) ->
    [mine_block(Block) | Chain].
	
mine_block(Block) ->
    mine_block(Block, 0, false).

mine_block(Block, Nonce, true) ->
    Block.
mine_block(Block, Nonce, Success) ->
    MiningBlock   = Block#block{nonce = Nonce},
    Hash          = hash_block(MiningBlock),
    Success       = binary:at(Hash, 0) == 48,
    mine_block(MiningBlock#block{hash = Hash}, Nonce, Success).
	




		
