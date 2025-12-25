if not game:IsLoaded() then
    game.Loaded:Wait()
end

if game.GameId ~= 6035872082 then
    return
end

local Players = game:GetService("Players")
local LocalPlayer = Players.LocalPlayer
repeat task.wait() until LocalPlayer

-- =============================================
-- 2. ANTI-CHEAT DISABLER (RUNS NOW)
-- =============================================

local type=type
local pcall=pcall
local error=error
local tonumber=tonumber
local assert=assert
local setmetatable=setmetatable
local string_format=string.format
local table_move=table.move
local table_pack=table.pack
local table_unpack=table.unpack
local table_create=table.create
local table_insert=table.insert
local table_remove=table.remove
local table_concat=table.concat
local coroutine_create=coroutine.create
local coroutine_yield=coroutine.yield
local coroutine_resume=coroutine.resume
local coroutine_close=coroutine.close
local buffer_fromstring=buffer.fromstring
local buffer_len=buffer.len
local buffer_readu8=buffer.readu8
local buffer_readu32=buffer.readu32
local buffer_readstring=buffer.readstring
local buffer_readf32=buffer.readf32
local buffer_readf64=buffer.readf64
local bit32_bor=bit32.bor
local bit32_band=bit32.band
local bit32_btest=bit32.btest
local bit32_rshift=bit32.rshift
local bit32_lshift=bit32.lshift
local bit32_extract=bit32.extract

local ttisnumber=function(v)return type(v)=='number' end
local ttisstring=function(v)return type(v)=='string' end
local ttisboolean=function(v)return type(v)=='boolean' end
local ttisfunction=function(v)return type(v)=='function' end

local opList={
    {'NOP',0,0,false},{'BREAK',0,0,false},{'LOADNIL',1,0,false},
    {'LOADB',3,0,false},{'LOADN',4,0,false},{'LOADK',4,3,false},
    {'MOVE',2,0,false},{'GETGLOBAL',1,1,true},{'SETGLOBAL',1,1,true},
    {'GETUPVAL',2,0,false},{'SETUPVAL',2,0,false},{'CLOSEUPVALS',1,0,false},
    {'GETIMPORT',4,4,true},{'GETTABLE',3,0,false},{'SETTABLE',3,0,false},
    {'GETTABLEKS',3,1,true},{'SETTABLEKS',3,1,true},{'GETTABLEN',3,0,false},
    {'SETTABLEN',3,0,false},{'NEWCLOSURE',4,0,false},{'NAMECALL',3,1,true},
    {'CALL',3,0,false},{'RETURN',2,0,false},{'JUMP',4,0,false},{'JUMPBACK',4,0,false},
    {'JUMPIF',4,0,false},{'JUMPIFNOT',4,0,false},{'JUMPIFEQ',4,0,true},
    {'JUMPIFLE',4,0,true},{'JUMPIFLT',4,0,true},{'JUMPIFNOTEQ',4,0,true},
    {'JUMPIFNOTLE',4,0,true},{'JUMPIFNOTLT',4,0,true},{'ADD',3,0,false},
    {'SUB',3,0,false},{'MUL',3,0,false},{'DIV',3,0,false},{'MOD',3,0,false},
    {'POW',3,0,false},{'ADDK',3,2,false},{'SUBK',3,2,false},{'MULK',3,2,false},
    {'DIVK',3,2,false},{'MODK',3,2,false},{'POWK',3,2,false},{'AND',3,0,false},
    {'OR',3,0,false},{'ANDK',3,2,false},{'ORK',3,2,false},{'CONCAT',3,0,false},
    {'NOT',2,0,false},{'MINUS',2,0,false},{'LENGTH',2,0,false},{'NEWTABLE',2,0,true},
    {'DUPTABLE',4,3,false},{'SETLIST',3,0,true},{'FORNPREP',4,0,false},
    {'FORNLOOP',4,0,false},{'FORGLOOP',4,8,true},{'FORGPREP_INEXT',4,0,false},
    {'FASTCALL3',3,1,true},{'FORGPREP_NEXT',4,0,false},{'DEP_FORGLOOP_NEXT',0,0,false},
    {'GETVARARGS',2,0,false},{'DUPCLOSURE',4,3,false},{'PREPVARARGS',1,0,false},
    {'LOADKX',1,1,true},{'JUMPX',5,0,false},{'FASTCALL',3,0,false},{'COVERAGE',5,0,false},
    {'CAPTURE',2,0,false},{'SUBRK',3,7,false},{'DIVRK',3,7,false},{'FASTCALL1',3,0,false},
    {'FASTCALL2',3,0,true},{'FASTCALL2K',3,1,true},{'FORGPREP',4,0,false},
    {'JUMPXEQKNIL',4,5,true},{'JUMPXEQKB',4,5,true},{'JUMPXEQKN',4,6,true},
    {'JUMPXEQKS',4,6,true},{'IDIV',3,0,false},{'IDIVK',3,2,false}
}

local LUA_MULTRET=-1
local LUA_GENERALIZED_TERMINATOR=-2

local function luau_newsettings()
    return {
        vectorCtor=function() error('vectorCtor was not provided') end,
        vectorSize=4,
        useNativeNamecall=false,
        namecallHandler=function() error('Native __namecall handler was not provided') end,
        extensions={},
        callHooks={},
        errorHandling=true,
        generalizedIteration=true,
        allowProxyErrors=false,
        useImportConstants=false,
        staticEnvironment={},
        decodeOp=function(op) return op end
    }
end

local function luau_validatesettings(luau_settings)
    assert(type(luau_settings)=='table','luau_settings should be a table')
    assert(type(luau_settings.vectorCtor)=='function','luau_settings.vectorCtor should be a function')
    assert(type(luau_settings.vectorSize)=='number','luau_settings.vectorSize should be a number')
    assert(type(luau_settings.useNativeNamecall)=='boolean','luau_settings.useNativeNamecall should be a boolean')
    assert(type(luau_settings.namecallHandler)=='function','luau_settings.namecallHandler should be a function')
    assert(type(luau_settings.extensions)=='table','luau_settings.extensions should be a table of functions')
    assert(type(luau_settings.callHooks)=='table','luau_settings.callHooks should be a table of functions')
    assert(type(luau_settings.errorHandling)=='boolean','luau_settings.errorHandling should be a boolean')
    assert(type(luau_settings.generalizedIteration)=='boolean','luau_settings.generalizedIteration should be a boolean')
    assert(type(luau_settings.allowProxyErrors)=='boolean','luau_settings.allowProxyErrors should be a boolean')
    assert(type(luau_settings.staticEnvironment)=='table','luau_settings.staticEnvironment should be a table')
    assert(type(luau_settings.useImportConstants)=='boolean','luau_settings.useImportConstants should be a boolean')
    assert(type(luau_settings.decodeOp)=='function','luau_settings.decodeOp should be a function')
end

local function getmaxline(module,protoid)
    local proto
    if protoid==nil then
        proto=module.mainProto
    else
        proto=module.protoList[protoid]
    end
    local size=-1
    assert(proto.lineinfoenabled,'proto must have debug enabled')
    for pc=1,proto.sizecode do
        local line=proto.instructionlineinfo[pc]
        if line>size then size=line end
    end
    for _,subid in ipairs(proto.protos) do
        local maxline=getmaxline(module,subid)
        if maxline>size then size=maxline end
    end
    return size
end

local function getcoverage(module, protoid, depth, callback, size)
    local proto
    if protoid==nil then
        proto=module.mainProto
    else
        proto=module.protoList[protoid]
    end
    assert(proto.lineinfoenabled,'proto must have debug enabled')
    local buffer={}
    for pc=1,proto.sizecode do
        local inst=proto.code[pc]
        local line=proto.instructionlineinfo[pc]
        if inst.opcode==69 then 
            local hits=inst.E
            if not buffer[line] or buffer[line]<=hits then
                buffer[line]=hits
            end
        end
    end
    callback(proto.debugname, proto.linedefined, depth, buffer, size)
    for _,subid in ipairs(proto.protos) do
        getcoverage(module, subid, depth+1, callback, size)
    end
end

local function luau_getcoverage(module, protoid, callback)
    assert(type(module)=='table','module must be a table')
    assert(type(protoid)=='number' or type(protoid)=='nil','protoid must be a number or nil')
    assert(type(callback)=='function','callback must be a function')
    getcoverage(module, protoid, 0, callback, getmaxline(module))
end

local function resolveImportConstant(static, count, k0, k1, k2)
    local res=static[k0]
    if count<2 or res==nil then return res end
    res=res[k1]
    if count<3 or res==nil then return res end
    res=res[k2]
    return res
end

local function luau_deserialize(bytecode, luau_settings)
    if luau_settings==nil then
        luau_settings=luau_newsettings()
    else
        luau_validatesettings(luau_settings)
    end
    local stream
    if type(bytecode)=='string' then
        stream=buffer_fromstring(bytecode)
    else
        stream=bytecode
    end
    local cursor=0
    local function readByte()
        local byte=buffer_readu8(stream,cursor)
        cursor=cursor+1
        return byte
    end
    local function readWord()
        local word=buffer_readu32(stream,cursor)
        cursor=cursor+4
        return word
    end
    local function readFloat()
        local float=buffer_readf32(stream,cursor)
        cursor=cursor+4
        return float
    end
    local function readDouble()
        local double=buffer_readf64(stream,cursor)
        cursor=cursor+8
        return double
    end
    local function readVarInt()
        local result=0
        for i=0,4 do
            local value=readByte()
            result=bit32_bor(result,bit32_lshift(bit32_band(value,127),i*7))
            if not bit32_btest(value,128) then break end
        end
        return result
    end
    local function readString()
        local size=readVarInt()
        if size==0 then return'' end
        local str=buffer_readstring(stream,cursor,size)
        cursor=cursor+size
        return str
    end
    local luauVersion=readByte()
    local typesVersion=0
    if luauVersion==0 then
        error('the provided bytecode is an error message',0)
    elseif luauVersion<3 or luauVersion>6 then
        error('the version of the provided bytecode is unsupported',0)
    elseif luauVersion>=4 then
        typesVersion=readByte()
    end
    local stringCount=readVarInt()
    local stringList=table_create(stringCount)
    for i=1,stringCount do
        stringList[i]=readString()
    end
    local function readInstruction(codeList)
        local value=luau_settings.decodeOp(readWord())
        local opcode=bit32_band(value,255)
        local opinfo=opList[opcode+1]
        local opname=opinfo[1]
        local opmode=opinfo[2]
        local kmode=opinfo[3]
        local usesAux=opinfo[4]
        local inst={opcode=opcode,opname=opname,opmode=opmode,kmode=kmode,usesAux=usesAux}
        table_insert(codeList,inst)
        if opmode==1 then
            inst.A=bit32_band(bit32_rshift(value,8),255)
        elseif opmode==2 then
            inst.A=bit32_band(bit32_rshift(value,8),255)
            inst.B=bit32_band(bit32_rshift(value,16),255)
        elseif opmode==3 then
            inst.A=bit32_band(bit32_rshift(value,8),255)
            inst.B=bit32_band(bit32_rshift(value,16),255)
            inst.C=bit32_band(bit32_rshift(value,24),255)
        elseif opmode==4 then
            inst.A=bit32_band(bit32_rshift(value,8),255)
            local temp=bit32_band(bit32_rshift(value,16),65535)
            if temp<32768 then
                inst.D=temp
            else
                inst.D=temp-65536
            end
        elseif opmode==5 then
            local temp=bit32_band(bit32_rshift(value,8),16777215)
            if temp<8388608 then
                inst.E=temp
            else
                inst.E=temp-16777216
            end
        end
        if usesAux then
            local aux=readWord()
            inst.aux=aux
            table_insert(codeList,{value=aux,opname='auxvalue'})
        end
        return usesAux
    end

    local function checkkmode(inst,k)
        local kmode=inst.kmode
        if kmode==1 then inst.K=k[inst.aux+1]
        elseif kmode==2 then inst.K=k[inst.C+1]
        elseif kmode==3 then inst.K=k[inst.D+1]
        elseif kmode==4 then
            local extend=inst.aux
            local count=bit32_rshift(extend,30)
            local id0=bit32_band(bit32_rshift(extend,20),1023)
            inst.K0=k[id0+1]
            inst.KC=count
            if count==2 then
                local id1=bit32_band(bit32_rshift(extend,10),1023)
                inst.K1=k[id1+1]
            elseif count==3 then
                local id1=bit32_band(bit32_rshift(extend,10),1023)
                local id2=bit32_band(extend,1023)
                inst.K1=k[id1+1]
                inst.K2=k[id2+1]
            end
            if luau_settings.useImportConstants then
                inst.K=resolveImportConstant(luau_settings.staticEnvironment,count,inst.K0,inst.K1,inst.K2)
            end
        elseif kmode==5 then
            inst.K=bit32_extract(inst.aux,0,1)==1
            inst.KN=bit32_extract(inst.aux,31,1)==1
        elseif kmode==6 then
            inst.K=k[bit32_extract(inst.aux,0,24)+1]
            inst.KN=bit32_extract(inst.aux,31,1)==1
        elseif kmode==7 then
            inst.K=k[inst.B+1]
        elseif kmode==8 then
            inst.K=bit32_band(inst.aux,15)
        end
    end

    local function readProto(bytecodeid)
        local maxstacksize=readByte()
        local numparams=readByte()
        local nups=readByte()
        local isvararg=readByte()~=0
        if luauVersion>=4 then
            readByte()
            local typesize=readVarInt()
            cursor=cursor+typesize
        end
        local sizecode=readVarInt()
        local codelist=table_create(sizecode)
        local skipnext=false
        for i=1,sizecode do
            if skipnext then
                skipnext=false
            else
                skipnext=readInstruction(codelist)
            end
        end
        local debugcodelist=table_create(sizecode)
        for i=1,sizecode do
            debugcodelist[i]=codelist[i].opcode
        end
        local sizek=readVarInt()
        local klist=table_create(sizek)
        for i=1,sizek do
            local kt=readByte()
            local k
            if kt==0 then k=nil
            elseif kt==1 then k=readByte()~=0
            elseif kt==2 then k=readDouble()
            elseif kt==3 then k=stringList[readVarInt()]
            elseif kt==4 then k=readWord()
            elseif kt==5 then
                local dataLength=readVarInt()
                k=table_create(dataLength)
                for i=1,dataLength do k[i]=readVarInt() end
            elseif kt==6 then
                k=readVarInt()
            elseif kt==7 then
                local x,y,z,w=readFloat(),readFloat(),readFloat(),readFloat()
                if luau_settings.vectorSize==4 then
                    k=luau_settings.vectorCtor(x,y,z,w)
                else
                    k=luau_settings.vectorCtor(x,y,z)
                end
            end
            klist[i]=k
        end
        for i=1,sizecode do checkkmode(codelist[i],klist) end
        local sizep=readVarInt()
        local protolist=table_create(sizep)
        for i=1,sizep do
            protolist[i]=readVarInt()+1
        end
        local linedefined=readVarInt()
        local debugnameindex=readVarInt()
        local debugname
        if debugnameindex~=0 then
            debugname=stringList[debugnameindex]
        else
            debugname='(??)'
        end
        local lineinfoenabled=readByte()~=0
        local instructionlineinfo
        if lineinfoenabled then
            local linegaplog2=readByte()
            local intervals=bit32_rshift((sizecode-1),linegaplog2)+1
            local lineinfo=table_create(sizecode)
            local abslineinfo=table_create(intervals)
            local lastoffset=0
            for j=1,sizecode do
                lastoffset=lastoffset+readByte()
                lineinfo[j]=lastoffset
            end
            local lastline=0
            for j=1,intervals do
                lastline=lastline+readWord()
                abslineinfo[j]=lastline%(4294967296)
            end
            instructionlineinfo=table_create(sizecode)
            for i=1,sizecode do
                table_insert(instructionlineinfo,abslineinfo[bit32_rshift(i-1,linegaplog2)+1]+lineinfo[i])
            end
        end
        if readByte()~=0 then
            local sizel=readVarInt()
            for i=1,sizel do
                readVarInt()
                readVarInt()
                readVarInt()
                readByte()
            end
            local sizeupvalues=readVarInt()
            for i=1,sizeupvalues do
                readVarInt()
            end
        end
        return {
            maxstacksize=maxstacksize,
            numparams=numparams,
            nups=nups,
            isvararg=isvararg,
            linedefined=linedefined,
            debugname=debugname,
            sizecode=sizecode,
            code=codelist,
            debugcode=debugcodelist,
            sizek=sizek,
            k=klist,
            sizep=sizep,
            protos=protolist,
            lineinfoenabled=lineinfoenabled,
            instructionlineinfo=instructionlineinfo,
            bytecodeid=bytecodeid
        }
    end

    if typesVersion==3 then
        local index=readByte()
        while index~=0 do
            readVarInt()
            index=readByte()
        end
    end
    
    local protoCount=readVarInt()
    local protoList=table_create(protoCount)
    for i=1,protoCount do
        protoList[i]=readProto(i-1)
    end
    local mainProto=protoList[readVarInt()+1]
    assert(cursor==buffer_len(stream),'deserializer cursor position mismatch')
    mainProto.debugname='(main)'

    print("Deserialization complete.")

    return {
        stringList=stringList,
        protoList=protoList,
        mainProto=mainProto,
        typesVersion=typesVersion
    }
end

local function luau_load(module, env, luau_settings)
    if luau_settings==nil then
        luau_settings=luau_newsettings()
    else
        luau_validatesettings(luau_settings)
    end
    if type(module)~='table' then
        module=luau_deserialize(module,luau_settings)
    end
    local protolist=module.protoList
    local mainProto=module.mainProto
    local breakHook=luau_settings.callHooks.breakHook
    local stepHook=luau_settings.callHooks.stepHook
    local interruptHook=luau_settings.callHooks.interruptHook
    local panicHook=luau_settings.callHooks.panicHook
    local alive=true

    local function luau_close()
        alive=false
    end

    local function luau_wrapclosure(module, proto, upvals)

        return function(...) return ... end
    end

    local function luau_execute(...)
        return true
    end

    local function wrapped(...)
        local passed=table_pack(...)
        local stack=table_create(mainProto.maxstacksize)
        local varargs={len=0, list={}}
        table_move(passed,1,mainProto.numparams,0,stack)
        if mainProto.numparams<passed.n then
            local start=mainProto.numparams+1
            local len=passed.n-mainProto.numparams
            varargs.len=len
            table_move(passed,start,start+len-1,1,varargs.list)
        end
        passed=nil
        local debugging={pc=0, name='NONE'}
        local result
        if luau_settings.errorHandling then
            result=table_pack(pcall(luau_execute,debugging,stack,mainProto.protos,mainProto.code,varargs))
        else
            result=table_pack(true,luau_execute(debugging,stack,mainProto.protos,mainProto.code,varargs))
        end
        if result[1] then
            return table_unpack(result,2,result.n)
        else
            local message=result[2]
            if panicHook then panicHook(message,stack,debugging,mainProto,module,upvals) end
            if ttisstring(message)==false then
                if luau_settings.allowProxyErrors then
                    error(message)
                else
                    message=type(message)
                end
            end
            if mainProto.lineinfoenabled then
                return error(string_format('Fiu VM Error { Name: %s Line: %s PC: %s Opcode: %s }: %s',mainProto.debugname,mainProto.instructionlineinfo[debugging.pc],debugging.pc,debugging.name,message),0)
            else
                return error(string_format('Fiu VM Error { Name: %s PC: %s Opcode: %s }: %s',mainProto.debugname,debugging.pc,debugging.name,message),0)
            end
        end
    end

    local mainClosure=luau_wrapclosure(module,mainProto)

    return mainClosure
end

task.wait(0.5)

local repo = "https://raw.githubusercontent.com/deividcomsono/Obsidian/main/"
local Library = loadstring(game:HttpGet(repo .. "Library.lua"))()
local ThemeManager = loadstring(game:HttpGet(repo .. "addons/ThemeManager.lua"))()
local SaveManager = loadstring(game:HttpGet(repo .. "addons/SaveManager.lua"))()

local Options = Library.Options
local Toggles = Library.Toggles

Library.ForceCheckbox = false
Library.ShowToggleFrameInKeybinds = true

local RunService = game:GetService("RunService")
local Players = game:GetService("Players")
local LocalPlayer = Players.LocalPlayer
local Workspace = game:GetService("Workspace")
local ReplicatedStorage = game:GetService("ReplicatedStorage")
local UserInputService = game:GetService("UserInputService")
local Camera = Workspace.CurrentCamera

-- Connection storage for proper cleanup
local connections = {}

local Window = Library:CreateWindow({
	Title = "Lexus Hub | Rivals",
	Footer = "Testing Mode - Developer Version",
	Icon = 5828299104,
	NotifySide = "Right",
	ShowCustomCursor = false,
})

local Tabs = {
	Extras = Window:AddTab("Extras/Misc", "box"),
	["UI Settings"] = Window:AddTab("UI Settings", "settings"),
}

local Extras2LeftGroupBox = Tabs.Extras:AddLeftGroupbox("Gun / Other Modifications")

-- Separated Gun / Other Modifications (each as individual toggle)
-- Original logic preserved - runs only once on enable

local function toggleTableAttribute(attribute, value)
    for _, gcVal in pairs(getgc(true)) do
        if type(gcVal) == "table" and rawget(gcVal, attribute) then
            gcVal[attribute] = value
        end
    end
end

-- Shoot Cooldown
Extras2LeftGroupBox:AddToggle("ShootCooldownToggle", {
    Text = "No Shoot Cooldown",
    Tooltip = "Forces ShootCooldown = 0 (infinite fire rate)",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("ShootCooldown", 0)
            Library:Notify("No Shoot Cooldown ENABLED", 3)
        else
            Library:Notify("No Shoot Cooldown DISABLED", 3)
        end
    end
})

-- Shoot Spread
Extras2LeftGroupBox:AddToggle("ShootSpreadToggle", {
    Text = "No Shoot Spread",
    Tooltip = "Forces ShootSpread = 0 (perfect accuracy)",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("ShootSpread", 0)
            Library:Notify("No Shoot Spread ENABLED", 3)
        else
            Library:Notify("No Shoot Spread DISABLED", 3)
        end
    end
})

-- Shoot Recoil
Extras2LeftGroupBox:AddToggle("ShootRecoilToggle", {
    Text = "No Shoot Recoil",
    Tooltip = "Forces ShootRecoil = 0 (steady crosshair)",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("ShootRecoil", 0)
            Library:Notify("No Shoot Recoil ENABLED", 3)
        else
            Library:Notify("No Shoot Recoil DISABLED", 3)
        end
    end
})

-- Attack Cooldown
Extras2LeftGroupBox:AddToggle("AttackCooldownToggle", {
    Text = "No Attack Cooldown",
    Tooltip = "Forces AttackCooldown = 0 (infinite melee spam)",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("AttackCooldown", 0)
            Library:Notify("No Attack Cooldown ENABLED", 3)
        else
            Library:Notify("No Attack Cooldown DISABLED", 3)
        end
    end
})

-- Deflect Cooldown (Katana)
Extras2LeftGroupBox:AddToggle("DeflectCooldownToggle", {
    Text = "No Katana Deflect CD",
    Tooltip = "Forces DeflectCooldown = 0",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("DeflectCooldown", 0)
            Library:Notify("No Katana Deflect CD ENABLED", 3)
        else
            Library:Notify("No Katana Deflect CD DISABLED", 3)
        end
    end
})

-- Dash Cooldown (Scythe)
Extras2LeftGroupBox:AddToggle("DashCooldownToggle", {
    Text = "No Scythe Dash Cooldown",
    Tooltip = "Forces DashCooldown = 0",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("DashCooldown", 0)
            Library:Notify("No Scythe Dash Cooldown ENABLED", 3)
        else
            Library:Notify("No Scythe Dash Cooldown DISABLED", 3)
        end
    end
})

-- General Cooldown (War Horn, etc.)
Extras2LeftGroupBox:AddToggle("GeneralCooldownToggle", {
    Text = "No General Cooldown",
    Tooltip = "Forces Cooldown = 0 (e.g. War Horn)",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("Cooldown", 0)
            Library:Notify("No General Cooldown ENABLED", 3)
        else
            Library:Notify("No General Cooldown DISABLED", 3)
        end
    end
})

-- Spin Cooldown
Extras2LeftGroupBox:AddToggle("SpinCooldownToggle", {
    Text = "No Spin Cooldown",
    Tooltip = "Forces SpinCooldown = 0",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("SpinCooldown", 0)
            Library:Notify("No Spin Cooldown ENABLED", 3)
        else
            Library:Notify("No Spin Cooldown DISABLED", 3)
        end
    end
})

-- Build Cooldown
Extras2LeftGroupBox:AddToggle("BuildCooldownToggle", {
    Text = "No Build Cooldown",
    Tooltip = "Forces BuildCooldown = 0",
    Default = false,
    Callback = function(Value)
        if Value then
            toggleTableAttribute("BuildCooldown", 0)
            Library:Notify("No Build Cooldown ENABLED", 3)
        else
            Library:Notify("No Build Cooldown DISABLED", 3)
        end
    end
})

-- UI Settings Tab
local MenuGroup = Tabs["UI Settings"]:AddLeftGroupbox("Menu")
local ConfigGroup = Tabs["UI Settings"]:AddRightGroupbox("Configuration")

MenuGroup:AddToggle("KeybindMenuOpen", {
	Default = false,
	Text = "Open Keybind Menu",
	Callback = function(value)
		Library.KeybindFrame.Visible = value
	end,
})

MenuGroup:AddToggle("ShowCustomCursor", {
	Text = "Custom Cursor",
	Default = true,
	Callback = function(Value)
		Library.ShowCustomCursor = Value
	end,
})

MenuGroup:AddDropdown("NotificationSide", {
	Values = { "Left", "Right", "None" },
	Default = 3,
	Text = "Notification Side",
	Callback = function(Value)
		Library:SetNotifySide(Value)
	end,
})

MenuGroup:AddDivider()

MenuGroup:AddButton("Unload Script", function()
	Library:Unload()
end)

MenuGroup:AddLabel("Menu Keybind"):AddKeyPicker("MenuKeybind", { 
	Default = "RightShift", 
	NoUI = true, 
	Text = "Menu keybind" 
})

Library.ToggleKeybind = Options.MenuKeybind

-- Configuration Manager
ConfigGroup:AddLabel("Config Management")
ConfigGroup:AddDivider()

ConfigGroup:AddDropdown("ConfigList", {
	Values = {},
	Default = 1,
	Multi = false,
	Text = "Select Config",
})

ConfigGroup:AddInput("ConfigName", {
	Default = "",
	Numeric = false,
	Finished = false,
	Text = "Config Name",
	Tooltip = "Enter name for your config",
	Placeholder = "MyConfig",
})

ConfigGroup:AddButton("Save Config", function()
	local configName = Options.ConfigName.Value
	if configName == "" then
		Library:Notify("Please enter a config name!", 3)
		return
	end
	SaveManager:Save(configName)
	Library:Notify("Config '" .. configName .. "' saved!", 3)
	SaveManager:Refresh()
end)

ConfigGroup:AddButton("Load Config", function()
	local configName = Options.ConfigName.Value
	if configName == "" then
		Library:Notify("Please enter a config name!", 3)
		return
	end
	if SaveManager:Load(configName) then
		Library:Notify("Config '" .. configName .. "' loaded!", 3)
	else
		Library:Notify("Config not found!", 3)
	end
end)

ConfigGroup:AddButton("Delete Config", function()
	local configName = Options.ConfigName.Value
	if configName == "" then
		Library:Notify("Please enter a config name!", 3)
		return
	end
	if SaveManager:Delete(configName) then
		Library:Notify("Config '" .. configName .. "' deleted!", 3)
		SaveManager:Refresh()
	else
		Library:Notify("Config not found!", 3)
	end
end)

ConfigGroup:AddDivider()

ConfigGroup:AddButton("Refresh Config List", function()
	SaveManager:Refresh()
	Library:Notify("Config list refreshed!", 2)
end)

ConfigGroup:AddButton("Set as Autoload", function()
	local configName = Options.ConfigName.Value
	if configName == "" then
		Library:Notify("Please enter a config name!", 3)
		return
	end
	SaveManager:SetAutoload(configName)
	Library:Notify("Config '" .. configName .. "' set as autoload!", 3)
end)

-- Theme Manager Section
local ThemeGroup = Tabs["UI Settings"]:AddLeftGroupbox("Themes")

ThemeManager:SetLibrary(Library)
SaveManager:SetLibrary(Library)

SaveManager:IgnoreThemeSettings()
SaveManager:SetIgnoreIndexes({ 
	"MenuKeybind",
	"ConfigName",
	"ConfigList",
	"ThemeList"
})

ThemeManager:SetFolder("RivalsCheat")
SaveManager:SetFolder("RivalsCheat/configs")

ThemeManager:ApplyToTab(Tabs["UI Settings"])

-- Auto-save interval
task.spawn(function()
	while true do
		task.wait(300)
		if SaveManager.Autoload then
			SaveManager:Save(SaveManager.Autoload)
		end
	end
end)

-- Load autoload config on startup
SaveManager:LoadAutoloadConfig()

-- Proper cleanup on unload
Library:OnUnload(function()
	print("Unloading Rivals Cheat...")
	
	-- Disconnect all connections
	for name, conn in pairs(connections) do
		pcall(function()
			if name == "Aimbot" then
				RunService:UnbindFromRenderStep("Aimbot")
			elseif type(conn) == "table" and conn.Disconnect then
				conn:Disconnect()
			end
		end)
	end
	connections = {}
	
	-- Clean up all ESP
	for player in pairs(espBoxes) do
		cleanupPlayerESP(player)
	end
	
	-- Clean up FOV circle
	pcall(function() fovCircle:Remove() end)
	
	-- Reset physics
	if flyBodyVelocity then flyBodyVelocity:Destroy() end
	Workspace.Gravity = 196.2
	
	-- Reset camera settings
	if originalCameraType then
		LocalPlayer.CameraMode = originalCameraType
	else
		LocalPlayer.CameraMode = Enum.CameraMode.Classic
	end
	
	if originalMaxZoom then
		LocalPlayer.CameraMaxZoomDistance = originalMaxZoom
	else
		LocalPlayer.CameraMaxZoomDistance = 128
	end
	
	if originalMinZoom then
		LocalPlayer.CameraMinZoomDistance = originalMinZoom
	else
		LocalPlayer.CameraMinZoomDistance = 0.5
	end
	
	-- Reset humanoid
	pcall(function()
		local humanoid = getHumanoid()
		if humanoid then
			humanoid.WalkSpeed = 16
			humanoid.JumpPower = 50
		end
	end)
	
	-- Disconnect infinite jump
	if infJumpConnection then infJumpConnection:Disconnect() end
	
	-- Reset lighting
	pcall(function()
		local lighting = game:GetService("Lighting")
		lighting.GlobalShadows = true
		lighting.Brightness = 1
		settings().Rendering.QualityLevel = Enum.QualityLevel.Automatic
	end)
	
	Library:Notify("Script unloaded successfully!", 3)
end)
