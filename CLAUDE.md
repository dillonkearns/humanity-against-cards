* Game name: **Humanity Against Cards**
* Elm + Lamdera architecture
* Tailwind v4 mobile-first UI
* Write/update lamdera/program-test cases for all core UI flows
* LLM deck seeding via admin JSON paste

---

### **Goal**

A mobile-first realtime party game (emphasis on social dynamic, the tech supports but doesn't take over the social element) similar to CAH. Players submit humorous cards to a prompt, a judge selects the winner, and players anonymously react to revealed answers.

The objective is **fun shared social chaos**, not competitive mastery.

---

### MVP Scope and simplifying assumptions

* **One game at a time** (no multi-room support)
* **One admin console** accessible at `/admin` (honor-system; not a player)
* **Players join at root `/`**, enter name, receive hand once /admin start button pressed, winner is shown once /admin end button is pressed
* **LLM not called in realtime**; card decks are pasted in via admin JSON
* **Durable identity and robust reconnect** via `localStorage` token
* **Lamdera program-test** for all core flows
* **Hand size: 10 cards** per player (replenished after submission)
* **No minimum players:** admin starts game when ready
* **Static player list:** once game starts, no join/leave mid-game (disconnects are transparent, no special handling)

---

### **Core Gameplay Loop**

1. **Join Lobby**

   * Enter name, get player identity via persistent token
   * Admin loads deck JSON (prompts + answers)

2. **Start Round**

   * Server deals hands
   * Judge rotates round-robin

3. **Prompt Phase**

   * Prompts cycle in order from deck
   * Judge has veto button to skip to next prompt

4. **Submission Phase**

   * Non-judge players choose one card

5. **Reveal Phase**

   * Judge UI (automatically what the current judge player sees) presses button manually to reveal submissions **one-by-one**
   * No reactions yet to preserve judge’s “performance moment”

6. **Reaction Phase**

   * Once all answers revealed:

     * Players tap emoji reactions (anonymous) so they have something interactive and fun to do while the judge is thinking
     * Judge **cannot see** reactions yet

7. **Judging**

   * Judge selects winner
   * Winner gets +1 point

8. **Post-Round**

   * Reaction totals are now revealed to **everyone**
   * Session stats updated (cumulative reaction counts per player)

9. **End Game**

   * Admin triggers end
   * Display session stats (raw reaction counts per player)

---

### **Reactions**

* Anonymous emoji reactions (e.g., 😂 😬 🤯)
* **One reaction per player per answer** (no editing, MVP)
* Reactions **hidden from judge until after winner is chosen**

---

### **Session Stats**

* Raw reaction counts accumulate across the game (e.g., total 😂, 😬, 🤯 received per player)
* Stats revealed on end game screen
* MVP: just show raw numbers; future enhancement: convert to awards like "Most Laughs", "Agent of Chaos"

---

### **Admin Screen (`/admin`)**

* NOT a player; separate UI
* Can:
  * Load deck JSON (this is generated from a manual prompt call to an LLM. The prompt template should be maintained in PROMPT.md, describing the JSON output format with single-blank prompts, and should accommodate a bullet list supplied by the admin at the top of the prompt to give some specific inside jokes and themes that should be included in the generated deck)
  * Start / end game
* No identity binding required
* Honor-system: only creator uses it

---

### **Deck Format**

Admin pastes JSON:

```json
{
  "prompts": [
    "J.K Rowling: Harry Potter and the Chamber of ____.",
    "Moms love ____."
  ],
  "answers": [
    "A wildly misguided plan with strong confidence.",
    "Free samples."
  ]
}
```

**MVP: Single-blank prompts only** (all prompts have exactly one `____`).
Future enhancement: support 2+ blanks.

*(LLM used manually outside of this app to generate this; no HTTP calls to LLM during play)*

---

### **Architecture**

* **Elm + Lamdera**
* **Tailwind v4** mobile-first
* Durable identity via stored `PlayerToken`
* Single global `Game` state on backend
* Judge rotation & round phases controlled server-side

---

### **Testing Requirement**

All critical flows tested with **`lamdera/program-test`**, including:

* Join, name, persistent identity
* Admin loads deck
* Start round
* Submit
* Reveal one-by-one
* Reaction phase unlock
* Judge selection
* Score increment
* Stats on end game
* Reconnect behavior
* **Admin never becomes a player**
* **Only one game exists**

---

### **UX Notes**

* Bottom-navigation touch controls
* Big buttons, thumb-first design
* Card flip animations for reveal
* Reactions appear only after all revealed
* No timers

---

### **Non-goals (MVP)**

* No spectator mode
* No public lobby / multiple rooms
* No profanity moderation (trusted groups)
* No real-time AI generation
* No timers or voice features

---

### **Tone**

Smart, social, playful.
Designed for **shared amusement**, inside jokes, comedic timing, and lightweight chaos.

> The judge’s reveal is the “stage.”
> The reactions are the “crowd.”
> The awards reinforce the group lore.
