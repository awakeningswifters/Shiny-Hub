local ReplicatedStorage = game:GetService("ReplicatedStorage")
local HurtEffect = require(Workspace:WaitForChild("HurtEffect"))
local Utility = require(ReplicatedStorage:WaitForChild("Modules"):WaitForChild("Utility"))
local Players = game:GetService("Players")
local Camera = workspace.CurrentCamera
local LocalPlayer = Players.LocalPlayer
local Character = LocalPlayer.Character or LocalPlayer.CharacterAdded:Wait()
local HumanoidRootPart = Character and Character:WaitForChild("HumanoidRootPart")

local function isTeammate(player)
    local character = player.Character
    return character and character:FindFirstChild("TeammateLabel")
end

-- trying to skid lol
