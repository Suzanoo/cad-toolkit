# 🧰 CAD Toolkit

A lightweight collection of practical AutoCAD tools for everyday engineering and CAD workflows.

The project focuses on small tools that solve real drafting problems with minimal setup:

> **Load → Run command → Get the job done**

Most tools are currently written in AutoLISP and can be loaded directly with AutoCAD `APPLOAD`.

---

## ✨ Tools

### 🔤 Font Doctor

**File:** `FONT_DOCTOR.lsp`  
**Command:** `FONTFIX`

Finds suspicious AutoCAD text styles and helps replace missing or problematic fonts.

Features:

- 🔎 Scans drawing text styles
- ⚠️ Detects missing or suspicious fonts
- 📊 Shows affected object counts
- 💡 Suggests replacement fonts
- 🔄 Provides alternative font choices
- 📂 Allows manual font selection
- 🛡️ Keeps the original style when no change is required

### 📍 Get N/E Coordinates

**File:** `GET_NE.lsp`  
**Command:** `GET_NE`

Pick points directly in AutoCAD and record their survey coordinates.

Coordinate convention:

- **X = Easting (E)**
- **Y = Northing (N)**

The tool creates persistent `NE_POINT_MARKER` objects containing:

- Point number
- Northing
- Easting

Coordinate data is stored as AutoCAD XData using:

```text
SURVEY_NE_DATA