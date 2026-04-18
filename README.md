# mocSY-formal-proof

**Formal Verification of Synchronous Systems for Aerospace Applications**

This repository contains the formal proof and property discovery framework for the **Attitude and Attitude Estimation System (AAES)**, modeled using the Synchronous Model of Computation (MoC) in **ForSyDe-Shallow**.

## 🚀 Overview

The project focuses on verifying critical behavioral properties of safety-critical synchronous systems. By leveraging **QuickCheck** for property testing and **QuickSpec** for property discovery, we ensure that the system model adheres to the fundamental hypotheses of the Synchronous MoC.

### Key Features

*   **AAES PN Model**: A complete implementation of an Attitude Estimation System including triple modular redundancy (TMR) and spare management.
*   **Formal Property Suite**: Implementation of $P_{SY1}$ through $P_{SY9}$ properties to validate synchronicity, determinism, and causality.
*   **Property Discovery**: Automated discovery of behavioral laws using algebraic exploration.
*   **Simulation Engine**: Execution environment for validating system response against IMU sensor inputs.

---

## 📂 Project Structure

| Path | Description |
| :--- | :--- |
| `src/Models/System.hs` | Implementation of the AAES system model and synchronous primitives. |
| `src/Verification/Properties.hs` | Formal definitions of synchronous properties ($P_{SYx}$). |
| `src/Discovery/Signatures.hs` | QuickSpec signatures for automated property exploration. |
| `app/Main.hs` | Entry point for the system simulation. |
| `app/RunDiscovery.hs` | Entry point for the property discovery engine. |
| `test/Spec.hs` | Test suite for property-based verification. |

---

## 🛠️ Getting Started

### Prerequisites

*   [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

### Build & Setup

```bash
# Update and install dependencies
stack build
```

### Execution Commands

| Task | Command |
| :--- | :--- |
| **Run Simulation** | `stack exec system-simulation` |
| **Property Discovery** | `stack exec properties-discovery` |
| **Run Test Suite** | `stack test` |

---

## ✅ Verified Properties

The following properties are formally verified against the AAES model:

*   **P_SY1 (Synchronous Hypothesis)**: Ensures a 1:1 logical tick mapping.
*   **P_SY2 (Absent Signals)**: Validates robustness against missing packets.
*   **P_SY3 (Determinism)**: Guarantees that identical inputs produce identical outputs.
*   **P_SY6 (Causality)**: Ensures current outputs do not depend on future inputs.
*   **P_SY7 (Concurrent Composition)**: Validates parallel consistency.
*   **P_SY9 (Prefix Causality)**: Confirms stream extensions preserve past states.

---

## 👨‍💻 Author

**Diego Rocha** - [diegodmr@ita.br](mailto:diegodmr@ita.br)
*Dissertation Project - ITA (Instituto Tecnológico de Aeronáutica)*