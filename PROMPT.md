# Custom Card Generation Prompt Template

**Purpose:** Generate a small set of custom cards based on your inside jokes to mix with official Cards Against Humanity cards.

**Workflow:**
1. Use the deck builder script (`elm-pages run script/src/DeckBuilder.elm`) to select and shuffle official CAH cards
2. Use this prompt to generate 15-25 custom cards specific to your group
3. Merge the JSON outputs
4. Paste the combined deck into the admin console

**Instructions:** Copy everything below the line into your LLM of choice (Claude, ChatGPT, etc.), and paste your custom themes/inside jokes at the top where indicated.

---

You are helping create custom cards for a party game called "Humanity Against Cards". These cards will be **mixed with official Cards Against Humanity cards**, so your job is to generate a small set (15-25 cards) that riff on specific inside jokes and themes relevant to this group of friends.

## Custom Themes & Inside Jokes

**[PASTE YOUR BULLET LIST HERE - examples below, replace with your own]**

- Our friend Dave's obsession with sourdough starter
- The time Sarah got locked in the escape room bathroom
- Mike's terrible dad jokes
- Our group's love of 90s nostalgia
- Jenny's competitive board game rage

## Output Format

Generate a JSON object with exactly this structure:

```json
{
  "prompts": [
    "Prompt with exactly one _ blank.",
    "Another prompt with one _."
  ],
  "answers": [
    "A short, punchy answer phrase.",
    "Another answer."
  ]
}
```

## Requirements

1. **Prompts:** 5-10 prompts, each with exactly ONE blank denoted by `_` (single underscore)
2. **Answers:** 15-20 answer phrases
3. **Tone:** Match the style of Cards Against Humanity - clever, absurd, irreverent
4. **Focus:** Every card should directly reference or riff on the inside jokes/themes listed above
5. **Single-blank only:** Every prompt must have exactly one `_` - no more, no less
6. **Answer style:** Short phrases (2-8 words typically), not complete sentences. Can be nouns, verbs, scenarios, or concepts
7. **Style matching:** Since these mix with official CAH cards, match their comedic timing and format
8. **Avoid:** Generic humor - these should only make sense to this specific group

## Good Examples

**Prompts:**
- "TSA guidelines now prohibit _ on airplanes."
- "Science cannot explain _."
- "Life's pretty tough in the fast lane. That's why I never leave the house without _."
- "This is the way the world ends. Not with a bang, but with _."

**Answers:**
- "Unresolved childhood trauma"
- "A concerning amount of confidence"
- "Aggressive finger guns"
- "The exact wrong energy for this situation"
- "A solution that creates three new problems"

## Bad Examples (Don't do this)

**Prompts with wrong blank count:**
- "Mixing ____ with ____ creates magic." ❌ (two blanks)
- "The answer is always cats." ❌ (no blank)

**Answers that don't work:**
- "I went to the store and bought some milk and eggs." ❌ (too long, too literal)
- "Yes" ❌ (too short, not funny)

## Context for Tone

This game is about:
- Creating absurd juxtapositions
- Inside jokes and shared cultural references
- Moments of unexpected hilarity
- Social bonding through shared laughter

**Not** about:
- Being intentionally cruel or mean-spirited
- Shock value for its own sake
- Punching down at vulnerable groups

## Your Task

Generate 5-10 prompts and 15-20 answers that **directly reference** the inside jokes and themes from the bullet list above. These will be mixed with ~180 official Cards Against Humanity cards, so focus on making cards that are hyper-specific to this group's context.

**Example approach:**
- If the list mentions "Dave's sourdough starter obsession" → create prompts/answers about sourdough, fermentation, Dave's personality
- If the list mentions "escape room bathroom incident" → create cards that reference that specific story

Return ONLY valid JSON - no other text, explanation, or markdown code fences around it.
