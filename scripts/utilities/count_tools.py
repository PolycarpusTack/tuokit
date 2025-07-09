import json

# Complete tool registry
COMPLETE_TOOL_REGISTRY = {
    "🧠 Code Intelligence": 7,
    "🗄️ Data & SQL": 5,
    "💎 Ruby & Rails": 14,
    "🦆 SmallTalk Tools": 7,
    "🤖 Agent Systems": 4,
    "🎨 UI & Components": 3,
    "📚 Learning & Documents": 5,
    "💾 Knowledge & Help": 3
}

total = sum(COMPLETE_TOOL_REGISTRY.values())
print(f"Total tools in TuoKit: {total}")
print("\nBreakdown by category:")
for category, count in COMPLETE_TOOL_REGISTRY.items():
    print(f"  {category}: {count} tools")
