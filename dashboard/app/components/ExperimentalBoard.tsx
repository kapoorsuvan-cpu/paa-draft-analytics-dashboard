"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import type { CSSProperties } from "react";
import {
  ArrowDown, ArrowUp, BarChart3, FlaskConical, Search, X
} from "lucide-react";

type Player = Record<string, unknown> & {
  id: number; rank: number; name: string; school: string; position: string;
  eligibility: string;
  draftProbability: number; threshold: number; projectedDrafted: boolean;
  projectedRange: string; tier: string; confidence: number;
  probR1: number; probR23: number; probR45: number; probR67: number;
  height: number | null; weight: number | null; headshot: string | null;
};

type Feature = {
  position: string; feature: string; weight: number; spearman: number; n: number;
  mean: number; sd: number; median: number; min: number; max: number;
};

type Metric = Record<string, string | number>;

type ExperimentalBoardProps = {
  players: Player[];
  features: Feature[];
  roundMetrics: Metric[];
  positionMetrics: Metric[];
};

const pct = (value: number, digits = 0) => `${(value * 100).toFixed(digits)}%`;
const num = (value: unknown, digits = 1) => {
  const parsed = Number(value);
  return Number.isFinite(parsed)
    ? parsed.toLocaleString(undefined, { maximumFractionDigits: digits })
    : "—";
};
const featureLabel = (value: string) => value
  .replace(/^so_/, "Sophomore ")
  .replace(/_pct$/, " percentile")
  .replaceAll("_", " ")
  .replace(/\b\w/g, character => character.toUpperCase());
const tierLabel = (tier: string) => tier === "R1"
  ? "Round 1"
  : tier === "R2_3"
    ? "Rounds 2-3"
    : tier === "R4_5"
      ? "Rounds 4-5"
      : "Rounds 6-7";

function ProbabilityBar({
  name, value, active
}: {
  name: string; value: number; active?: boolean;
}) {
  return <div className={`probRow ${active ? "active" : ""}`}>
    <div><span>{name}</span><strong>{pct(value)}</strong></div>
    <div className="track"><i style={{ width: `${value * 100}%` }} /></div>
  </div>;
}

export function ExperimentalBoard({
  players, features, roundMetrics, positionMetrics
}: ExperimentalBoardProps) {
  const [query, setQuery] = useState("");
  const [position, setPosition] = useState("All");
  const [selectedId, setSelectedId] = useState<number | null>(null);
  const [cardHeight, setCardHeight] = useState<number | null>(null);
  const cardRef = useRef<HTMLElement>(null);
  const positions = useMemo(
    () => [...new Set(players.map(player => player.position))].sort(),
    [players]
  );
  const filtered = useMemo(() => {
    const normalizedQuery = query.toLowerCase().trim();
    return players
      .filter(player => position === "All" || player.position === position)
      .filter(player => !normalizedQuery ||
        `${player.name} ${player.school} ${player.position}`
          .toLowerCase()
          .includes(normalizedQuery));
  }, [players, position, query]);
  const selected = filtered.find(player => player.id === selectedId) ??
    filtered[0] ?? players[0];
  const selectedFeatures = features.filter(
    feature => feature.position === selected?.position
  );
  const bestMetric = roundMetrics.find(
    metric => metric.method === "tabfm_augmented"
  );
  const holdoutDrafted = positionMetrics.reduce(
    (sum, metric) => sum + Number(metric.drafted ?? 0),
    0
  );
  const maxFeatureWeight = Math.max(
    1e-9,
    ...selectedFeatures.map(feature => feature.weight)
  );

  useEffect(() => {
    const card = cardRef.current;
    if (!card) return;
    const updateHeight = () => setCardHeight(Math.ceil(card.getBoundingClientRect().height));
    updateHeight();
    const observer = new ResizeObserver(updateHeight);
    observer.observe(card);
    return () => observer.disconnect();
  }, [selected?.id, selectedFeatures.length]);

  if (!players.length) return null;

  return <section id="junior-board" className="section juniorSection">
    <div className="sectionIntro">
      <div><span className="sectionNumber">02</span><h2>Rising Junior Board</h2></div>
      <p>Sophomore-season profiles scored with the same two-stage method and a separately trained TabFM round model.</p>
    </div>
    <div className="experimentalNote">
      <FlaskConical size={21}/>
      <div>
        <strong>Experimental sophomore-year projection</strong>
        <p>This is not a projection of who will enter the next draft class. It estimates eventual draft outcomes using information available after the sophomore season. It cannot anticipate many breakouts, role changes, transfers, injuries, testing results, or later development, so it is less reliable than the rising-senior board.</p>
      </div>
      <dl>
        <div><dt>Exact tier</dt><dd>{pct(Number(bestMetric?.exact_tier_accuracy ?? 0), 1)}</dd></div>
        <div><dt>Within one tier</dt><dd>{pct(Number(bestMetric?.within_one_tier_accuracy ?? 0), 1)}</dd></div>
        <div><dt>Holdout drafted</dt><dd>{holdoutDrafted.toLocaleString()}</dd></div>
      </dl>
    </div>
    <div className="boardLayout">
      <div className={`boardPanel${cardHeight ? " juniorBoardPanel" : ""}`} style={cardHeight ? ({ "--detail-height": `${cardHeight}px` } as CSSProperties) : undefined}>
        <div className="filters">
          <label className="search">
            <Search size={17}/>
            <input value={query} onChange={event => setQuery(event.target.value)} placeholder="Search rising junior or school" />
            {query && <button onClick={() => setQuery("")} aria-label="Clear search"><X size={15}/></button>}
          </label>
          <select aria-label="Filter rising juniors by position" value={position} onChange={event => setPosition(event.target.value)}>
            <option>All</option>{positions.map(value => <option key={value}>{value}</option>)}
          </select>
        </div>
        <div className="tableWrap">
          <table>
            <thead><tr><th>Rank</th><th>Prospect</th><th>Pos</th><th>Draft confidence</th><th>Model projection</th></tr></thead>
            <tbody>{filtered.map(player => <tr key={player.id} className={selected?.id === player.id ? "selected" : ""} onClick={() => setSelectedId(player.id)}>
              <td className="rank">{player.rank}</td>
              <td><button className="prospectSelect" onClick={event => { event.stopPropagation(); setSelectedId(player.id); }}><strong>{player.name}</strong><span>{player.school}</span></button></td>
              <td><b className="pos">{player.position}</b></td>
              <td><div className="probCell"><i style={{width:`${player.draftProbability*100}%`}}/><span>{pct(player.draftProbability)}</span></div></td>
              <td><strong>{player.projectedRange}</strong></td>
            </tr>)}</tbody>
          </table>
        </div>
        <div className="tableFoot">Showing {filtered.length} of {players.length} rising juniors · draft confidence first, then conditional round range if drafted</div>
      </div>

      {selected && <aside className="prospectCard juniorCard" ref={cardRef}>
        <div className="prospectTop">
          {selected.headshot ? <img src={selected.headshot} alt=""/> : <div className="avatar">{selected.name.split(" ").map(part => part[0]).join("").slice(0, 2)}</div>}
          <div><span className="rankLabel">#{selected.rank} · {selected.eligibility}</span><h3>{selected.name}</h3><p>{selected.school} · {selected.position}{selected.height ? ` · ${selected.height} in` : ""}{selected.weight ? ` · ${selected.weight} lb` : ""}</p></div>
        </div>
        <div className="projectionCallout">
          <div><span>Model projection:</span><strong>{selected.projectedRange}</strong></div>
          <div><span>{selected.projectedDrafted ? "Range confidence:" : "If drafted:"}</span><strong>{selected.projectedDrafted ? pct(selected.confidence) : tierLabel(selected.tier)}</strong></div>
        </div>
        <div className="draftChance"><span>Eventual draft confidence</span><strong>{pct(selected.draftProbability, 1)}</strong><div className="track"><i style={{width:`${selected.draftProbability*100}%`}}/></div><small>Experimental {selected.position} threshold: {pct(selected.threshold)}</small></div>
        <div className="probabilityLabel"><strong>Round range if drafted</strong><span>Conditional probabilities; together they equal 100%</span></div>
        <div className="probabilities">
          <ProbabilityBar name="Round 1" value={selected.probR1} active={selected.tier === "R1"}/>
          <ProbabilityBar name="Rounds 2–3" value={selected.probR23} active={selected.tier === "R2_3"}/>
          <ProbabilityBar name="Rounds 4–5" value={selected.probR45} active={selected.tier === "R4_5"}/>
          <ProbabilityBar name="Rounds 6–7" value={selected.probR67} active={selected.tier === "R6_7"}/>
        </div>
        <div className="featureHeader"><div><BarChart3 size={17}/><strong>Sophomore signal breakdown</strong></div><small>Historical draft-pick correlation by position</small></div>
        <div className="featureList">{selectedFeatures.map(feature => <div className="feature" key={feature.feature}>
          <div><span>{featureLabel(feature.feature)}</span><b>{pct(feature.weight)}</b></div>
          <div className="featureTrack"><i style={{width:`${feature.weight*100/maxFeatureWeight}%`}}/></div>
          <small>{num(selected[feature.feature], 2)} · {feature.spearman < 0 ? <><ArrowUp size={11}/> higher supports earlier selection</> : <><ArrowDown size={11}/> lower supports earlier selection</>}</small>
        </div>)}</div>
      </aside>}
    </div>
  </section>;
}
