"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import type { CSSProperties } from "react";
import {
  ArrowDown, ArrowUp, BarChart3, BookOpenCheck,
  Gauge, Info, Search, X
} from "lucide-react";
import { ExperimentalBoard } from "./components/ExperimentalBoard";

type Player = Record<string, unknown> & {
  id: number; rank: number; name: string; school: string; position: string;
  eligibility: string;
  draftProbability: number; threshold: number; projectedDrafted: boolean;
  projectedRange: string; tier: string; confidence: number;
  probR1: number; probR23: number; probR45: number; probR67: number;
  height: number | null;
  weight: number | null; headshot: string | null;
};

type Feature = {
  position: string; feature: string; weight: number; spearman: number; n: number;
  mean: number; sd: number; median: number; min: number; max: number;
};

type Metric = Record<string, string | number>;

const pct = (v: number, digits = 0) => `${(v * 100).toFixed(digits)}%`;
const num = (v: unknown, digits = 1) => {
  const n = Number(v);
  return Number.isFinite(n) ? n.toLocaleString(undefined, { maximumFractionDigits: digits }) : "—";
};
const label = (v: string) => v
  .replace(/^jr_/, "Junior ").replace(/^so_/, "Sophomore ")
  .replace(/^delta_/, "Change in ").replace(/_pct$/, " percentile")
  .replaceAll("_", " ").replace(/\b\w/g, c => c.toUpperCase());
const roundFromSlot = (slot: number) => Math.min(7, Math.max(1, Math.ceil(slot / 32)));
const rangeFromSlot = (slot: number) => slot <= 32 ? "Round 1" : slot <= 96 ? "Rounds 2–3" : slot <= 160 ? "Rounds 4–5" : "Rounds 6–7";
const DATA_VERSION = "20260720-r8";
const tierLabel = (tier: string) => tier === "R1" ? "Round 1" : tier === "R2_3" ? "Rounds 2-3" : tier === "R4_5" ? "Rounds 4-5" : "Rounds 6-7";

function ProbabilityBar({ name, value, active }: { name: string; value: number; active?: boolean }) {
  return <div className={`probRow ${active ? "active" : ""}`}>
    <div><span>{name}</span><strong>{pct(value)}</strong></div>
    <div className="track"><i style={{ width: `${value * 100}%` }} /></div>
  </div>;
}

export default function Home() {
  const [players, setPlayers] = useState<Player[]>([]);
  const [features, setFeatures] = useState<Feature[]>([]);
  const [roundMetrics, setRoundMetrics] = useState<Metric[]>([]);
  const [positionMetrics, setPositionMetrics] = useState<Metric[]>([]);
  const [juniorPlayers, setJuniorPlayers] = useState<Player[]>([]);
  const [sophomoreFeatures, setSophomoreFeatures] = useState<Feature[]>([]);
  const [sophomoreRoundMetrics, setSophomoreRoundMetrics] = useState<Metric[]>([]);
  const [sophomorePositionMetrics, setSophomorePositionMetrics] = useState<Metric[]>([]);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [query, setQuery] = useState("");
  const [position, setPosition] = useState("All");
  const [selectedId, setSelectedId] = useState<number | null>(null);
  const [scenarioPosition, setScenarioPosition] = useState("WR");
  const [scenarioValues, setScenarioValues] = useState<Record<string, number>>({});
  const [seniorCardHeight, setSeniorCardHeight] = useState<number | null>(null);
  const seniorCardRef = useRef<HTMLElement>(null);

  useEffect(() => {
    const controller = new AbortController();
    const loadJson = async (path: string) => {
      const response = await fetch(`${path}?v=${DATA_VERSION}`, {
        cache: "no-store",
        signal: controller.signal
      });
      if (!response.ok) throw new Error(`Could not load ${path}`);
      return response.json();
    };
    Promise.all([
      loadJson("/data/players.json"),
      loadJson("/data/features.json"),
      loadJson("/data/round_metrics.json"),
      loadJson("/data/position_metrics.json"),
      loadJson("/data/rising_juniors.json"),
      loadJson("/data/sophomore_features.json"),
      loadJson("/data/sophomore_round_metrics.json"),
      loadJson("/data/sophomore_position_metrics.json"),
    ]).then(([p, f, rm, pm, jp, sf, srm, spm]) => {
      setPlayers(p); setFeatures(f); setRoundMetrics(rm); setPositionMetrics(pm);
      setJuniorPlayers(jp); setSophomoreFeatures(sf);
      setSophomoreRoundMetrics(srm); setSophomorePositionMetrics(spm);
      setSelectedId(p[0]?.id ?? null);
    }).catch(error => {
      if (error instanceof DOMException && error.name === "AbortError") return;
      setLoadError(error instanceof Error ? error.message : "Could not load the draft boards");
    });
    return () => controller.abort();
  }, []);

  const positions = useMemo(() => [...new Set(players.map(p => p.position))].sort(), [players]);
  const filtered = useMemo(() => {
    const q = query.toLowerCase().trim();
    return players.filter(p => position === "All" || p.position === position)
      .filter(p => !q || `${p.name} ${p.school} ${p.position}`.toLowerCase().includes(q));
  }, [players, position, query]);
  const selected = filtered.find(p => p.id === selectedId) ?? filtered[0] ?? players[0];
  const selectedFeatures = features.filter(f => f.position === selected?.position);
  const bestMetric = roundMetrics.find(m => m.method === "tabfm_augmented");
  const baselineMetric = roundMetrics.find(m => m.method === "direct_multiclass");
  const scenarioFeatures = useMemo(
    () => features.filter(f => f.position === scenarioPosition),
    [features, scenarioPosition]
  );

  useEffect(() => {
    const next: Record<string, number> = {};
    features.filter(f => f.position === scenarioPosition).forEach(f => next[f.feature] = f.median);
    setScenarioValues(next);
  }, [scenarioPosition, features]);

  useEffect(() => {
    const card = seniorCardRef.current;
    if (!card) return;
    const updateHeight = () => setSeniorCardHeight(Math.ceil(card.getBoundingClientRect().height));
    updateHeight();
    const observer = new ResizeObserver(updateHeight);
    observer.observe(card);
    return () => observer.disconnect();
  }, [selected?.id, selectedFeatures.length]);

  const scenario = useMemo(() => {
    const fs = scenarioFeatures;
    const peerRounds = players
      .filter(p => p.position === scenarioPosition && p.projectedDrafted)
      .map(p => p.tier === "R1" ? 1 : p.tier === "R2_3" ? 2.5 : p.tier === "R4_5" ? 4.5 : 6.5)
      .sort((a, b) => a - b);
    const baseRound = peerRounds.length ? peerRounds[Math.floor(peerRounds.length / 2)] : 5;
    const base = (baseRound - 0.5) * 32;
    const score = fs.reduce((sum, f) => {
      const z = f.sd > 0 ? ((scenarioValues[f.feature] ?? f.median) - f.mean) / f.sd : 0;
      return sum + Math.max(-2.5, Math.min(2.5, z)) * f.weight * (f.spearman < 0 ? 1 : -1);
    }, 0);
    const slot = Math.round(Math.max(1, Math.min(257, base - score * 44)));
    return { slot, round: roundFromSlot(slot), range: rangeFromSlot(slot) };
  }, [players, scenarioFeatures, scenarioPosition, scenarioValues]);

  if (loadError) return <main className="loading" role="alert">{loadError}. Refresh the page to try again.</main>;
  if (!players.length) return <main className="loading"><div className="loader" />Loading the draft boards…</main>;

  return <div className="siteShell">
    <header className="topbar">
      <a className="brand" href="#top"><img src="/assets/paa-logo.png" alt="PAA" /><div><strong>PAA Draft Lab</strong><span>2027 Rising Senior Board</span></div></a>
      <nav><a href="#board">Senior Board</a><a href="#junior-board">Junior Board</a><a href="#projection">Projection Lab</a><a href="#quality">Quality</a></nav>
    </header>

    <main id="top">
      <section className="hero">
        <div className="heroCopy">
          <h1>2027 NFL<br/><em>Draft Board</em></h1>
          <p>PAA’s Data Analytics Team's  2026 rising juniors &amp; seniors, ordered by draft confidence and projected round range with position context and model probabilities.</p>
        </div>
        <div className="heroStats">
          <div className="stat accuracyStat featureStat"><div className="statLabel"><span>Exact tier accuracy</span></div><strong>{pct(Number(bestMetric?.exact_tier_accuracy ?? 0), 1)}</strong><small>Correct conditional round range</small><div className="statScale" aria-hidden="true"><i style={{width:`${Number(bestMetric?.exact_tier_accuracy ?? 0)*100}%`}}/></div></div>
          <div className="stat accuracyStat"><div className="statLabel"><span>Within one tier</span></div><strong>{pct(Number(bestMetric?.within_one_tier_accuracy ?? 0), 1)}</strong><small>Temporal 2022–23 validation</small><div className="statScale" aria-hidden="true"><i style={{width:`${Number(bestMetric?.within_one_tier_accuracy ?? 0)*100}%`}}/></div></div>
          <div className="stat boardStat"><div className="statLabel"><span>Verified prospects</span></div><strong>{(players.length + juniorPlayers.length).toLocaleString()}</strong><small>{players.length.toLocaleString()} rising seniors · {juniorPlayers.length.toLocaleString()} rising juniors</small><div className="boardCoverage"><i/><span>Two current eligibility boards</span></div></div>
          <div className="stat positionsStat"><div className="statLabel"><span>Positions covered</span></div><strong>{positions.length}</strong><small className="positionCodes">QB · RB · WR · TE<br/>EDGE · IDL · ILB · CB · SAF</small></div>
        </div>
      </section>

      <div className="accuracyNote"><BookOpenCheck size={20}/><div><p><strong>Before interpreting, read:</strong> Draft confidence estimates the chance that a player is selected in any round. A 20% value means about one in five similar historical profiles were drafted. Players below their position threshold are projected undrafted. Round probabilities apply only if the player is drafted. Multiply draft confidence by a round probability to estimate the overall chance for that range. Thresholds differ by position.</p><p><strong>Methodology:</strong> The project uses sophomore and junior production, usage, development, size, and school draft history. Position models estimate draft chance. A second model estimates round range for drafted players. Results were tested on a later-season holdout. The model does not know future injuries, role changes, transfers, declarations, testing results, or team needs.</p></div></div>

      <section id="board" className="section">
        <div className="sectionIntro"><div><span className="sectionNumber">01</span><h2>Rising Senior Board</h2></div><p>Search the updated board and open any prospect for the full probability profile.</p></div>
        <div className="boardLayout">
          <div className="boardPanel" style={seniorCardHeight ? ({ "--detail-height": `${seniorCardHeight}px` } as CSSProperties) : undefined}>
            <div className="filters">
              <label className="search"><Search size={17}/><input value={query} onChange={e => setQuery(e.target.value)} placeholder="Search player or school" />{query && <button onClick={() => setQuery("")} aria-label="Clear"><X size={15}/></button>}</label>
              <select aria-label="Filter rising seniors by position" value={position} onChange={e => setPosition(e.target.value)}><option>All</option>{positions.map(p => <option key={p}>{p}</option>)}</select>
            </div>
            <div className="tableWrap"><table><thead><tr><th>Rank</th><th>Prospect</th><th>Pos</th><th>Draft confidence</th><th>Model projection</th></tr></thead><tbody>{filtered.map(p => <tr key={p.id} className={selected?.id === p.id ? "selected" : ""} onClick={() => setSelectedId(p.id)}><td className="rank">{p.rank}</td><td><button className="prospectSelect" onClick={event => { event.stopPropagation(); setSelectedId(p.id); }}><strong>{p.name}</strong><span>{p.school}</span></button></td><td><b className="pos">{p.position}</b></td><td><div className="probCell"><i style={{width:`${p.draftProbability*100}%`}}/><span>{pct(p.draftProbability)}</span></div></td><td><strong>{p.projectedRange}</strong></td></tr>)}</tbody></table></div>
            <div className="tableFoot">Showing {filtered.length} prospects · actual two-stage model output: draft chance, then round range if drafted</div>
          </div>

          {selected && <aside className="prospectCard" ref={seniorCardRef}>
            <div className="prospectTop">{selected.headshot ? <img src={selected.headshot} alt=""/> : <div className="avatar">{selected.name.split(" ").map(x=>x[0]).join("").slice(0,2)}</div>}<div><span className="rankLabel">Board #{selected.rank} · {selected.eligibility}</span><h3>{selected.name}</h3><p>{selected.school} · {selected.position}{selected.height ? ` · ${selected.height} in` : ""}{selected.weight ? ` · ${selected.weight} lb` : ""}</p></div></div>
            <div className="projectionCallout"><div><span>Model projection:</span><strong>{selected.projectedRange}</strong></div><div><span>{selected.projectedDrafted ? "Range confidence:" : "If drafted:"}</span><strong>{selected.projectedDrafted ? pct(selected.confidence) : tierLabel(selected.tier)}</strong></div></div>
            <div className="draftChance"><span>Draft confidence</span><strong>{pct(selected.draftProbability, 1)}</strong><div className="track"><i style={{width:`${selected.draftProbability*100}%`}}/></div><small>Position threshold: {pct(selected.threshold)}</small></div>
            <div className="probabilityLabel"><strong>Round range if drafted</strong><span>Conditional probabilities; together they equal 100%</span></div>
            <div className="probabilities"><ProbabilityBar name="Round 1" value={selected.probR1} active={selected.tier === "R1"}/><ProbabilityBar name="Rounds 2–3" value={selected.probR23} active={selected.tier === "R2_3"}/><ProbabilityBar name="Rounds 4–5" value={selected.probR45} active={selected.tier === "R4_5"}/><ProbabilityBar name="Rounds 6–7" value={selected.probR67} active={selected.tier === "R6_7"}/></div>
            <div className="featureHeader"><div><BarChart3 size={17}/><strong>Feature weight breakdown</strong></div><small>Updated historical signal by position</small></div>
            <div className="featureList">{selectedFeatures.map(f => <div className="feature" key={f.feature}><div><span>{label(f.feature)}</span><b>{pct(f.weight)}</b></div><div className="featureTrack"><i style={{width:`${f.weight*100/Math.max(...selectedFeatures.map(x=>x.weight))}%`}}/></div><small>{num(selected[f.feature], 2)} · {f.spearman < 0 ? <><ArrowUp size={11}/> higher supports earlier selection</> : <><ArrowDown size={11}/> lower supports earlier selection</>}</small></div>)}</div>
          </aside>}
        </div>
      </section>

      <ExperimentalBoard players={juniorPlayers} features={sophomoreFeatures} roundMetrics={sophomoreRoundMetrics} positionMetrics={sophomorePositionMetrics}/>

      <section id="projection" className="section projectionSection">
        <div className="sectionIntro light"><div><span className="sectionNumber">03</span><h2>Custom Position Projection</h2></div><p>Set a rising senior’s junior-season profile and compare it with the historical position signal.</p></div>
        <div className="projectionGrid"><div className="scenarioForm"><label>Position<select value={scenarioPosition} onChange={e => setScenarioPosition(e.target.value)}>{positions.map(p => <option key={p}>{p}</option>)}</select></label><div className="scenarioInputs">{scenarioFeatures.map(f => <label key={f.feature}><span>{label(f.feature)} <b>{pct(f.weight)}</b></span><input type="number" step="any" value={scenarioValues[f.feature] ?? ""} onChange={e => setScenarioValues(v => ({...v,[f.feature]:Number(e.target.value)}))}/><small>Typical: {num(f.median,2)}</small></label>)}</div></div>
          <div className="scenarioResult"><div className="resultEyebrow"><Gauge size={17}/> Conditional round scenario</div><span className="rangeResult">{scenario.range}</span><strong>Round {scenario.round}</strong><p>This position comparison estimates round range among drafted historical peers. It does not estimate draft confidence or replace the full player model.</p><div className="scenarioScale"><span>Earlier</span><i><b style={{left:`${Math.min(96, Math.max(4, scenario.slot/257*100))}%`}}/></i><span>Later</span></div></div></div>
      </section>

      <section id="quality" className="section qualitySection">
        <div className="sectionIntro"><div><span className="sectionNumber">04</span><h2>Model Quality</h2></div></div>
        <div className="qualityGrid">
          <div className="qualityMain"><div className="qualityScore"><span>Validated exact tier accuracy</span><strong>{pct(Number(bestMetric?.exact_tier_accuracy ?? 0), 1)}</strong><small>vs. {pct(Number(baselineMetric?.exact_tier_accuracy ?? 0), 1)} baseline</small></div><div className="metricBars">{[
            ["Within one tier", Number(bestMetric?.within_one_tier_accuracy ?? 0)],
            ["Balanced accuracy", Number(bestMetric?.balanced_accuracy ?? 0)],
            ["Macro F1", Number(bestMetric?.macro_f1 ?? 0)]
          ].map(([n,v]) => <div key={String(n)}><span>{n}</span><b>{pct(Number(v),1)}</b><i><em style={{width:`${Number(v)*100}%`}}/></i></div>)}</div></div>
          <div className="qualityPositions"><h3>Draft classifier by position</h3><div>{positionMetrics.sort((a,b)=>Number(b.pr_auc)-Number(a.pr_auc)).map(m => <div key={String(m.position)}><b>{m.position}</b><span>PR AUC {num(m.pr_auc,3)}</span><i><em style={{width:`${Number(m.pr_auc)*100}%`}}/></i></div>)}</div></div>
          <div className="methodNote"><Info size={19}/><p><strong>Validation:</strong> draft classifier metrics are position-specific. Round-range metrics use drafted players only. This board is a relative projection, not a 257-selection mock draft.</p></div>
        </div>
      </section>
    </main>
    <footer><div className="brand"><img src="/assets/paa-logo.png" alt="PAA"/><div><strong>PAA Draft Lab</strong><span>Analytic driven draft projections</span></div></div></footer>
  </div>;
}
