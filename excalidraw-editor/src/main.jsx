import React, { useState, useEffect, useCallback } from "react";
import ReactDOM from "react-dom/client";
import { Excalidraw, THEME } from "@excalidraw/excalidraw";
import "@excalidraw/excalidraw/index.css";

// Read config from the server-injected global
const CONFIG = window.__EXCALIDRAW_CONFIG__ || {
  file: "drawing.excalidraw",
  theme: "light",
};

function App() {
  const [initialData, setInitialData] = useState(null);
  const [excalidrawAPI, setExcalidrawAPI] = useState(null);
  const [saving, setSaving] = useState(false);
  const [lastSaved, setLastSaved] = useState(null);

  // Load the .excalidraw file from the server
  useEffect(() => {
    fetch("/api/load")
      .then((r) => r.json())
      .then((data) => {
        setInitialData({
          elements: data.elements || [],
          appState: {
            ...(data.appState || {}),
            theme:
              CONFIG.theme === "dark" ? THEME.DARK : THEME.LIGHT,
            viewBackgroundColor:
              data.appState?.viewBackgroundColor || "#ffffff",
          },
          scrollToContent: true,
        });
      })
      .catch((err) => {
        console.error("Failed to load file:", err);
        setInitialData({
          elements: [],
          appState: {
            theme: CONFIG.theme === "dark" ? THEME.DARK : THEME.LIGHT,
            viewBackgroundColor: "#ffffff",
          },
          scrollToContent: true,
        });
      });
  }, []);

  // Save handler: POST current scene to the server
  const saveToServer = useCallback(async () => {
    if (!excalidrawAPI) return;
    setSaving(true);
    try {
      const elements = excalidrawAPI.getSceneElements();
      const appState = excalidrawAPI.getAppState();
      const payload = {
        type: "excalidraw",
        version: 2,
        source: "excalidraw-export",
        elements: elements,
        appState: {
          viewBackgroundColor: appState.viewBackgroundColor,
          gridSize: appState.gridSize || null,
        },
      };
      const resp = await fetch("/api/save", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      if (resp.ok) {
        setLastSaved(new Date().toLocaleTimeString());
      } else {
        console.error("Save failed:", resp.statusText);
      }
    } catch (err) {
      console.error("Save error:", err);
    } finally {
      setSaving(false);
    }
  }, [excalidrawAPI]);

  // Done handler: save then tell server we're finished
  const handleDone = useCallback(async () => {
    if (!excalidrawAPI) return;
    const elements = excalidrawAPI.getSceneElements();
    const appState = excalidrawAPI.getAppState();
    const payload = {
      type: "excalidraw",
      version: 2,
      source: "excalidraw-export",
      elements: elements,
      appState: {
        viewBackgroundColor: appState.viewBackgroundColor,
        gridSize: appState.gridSize || null,
      },
    };
    try {
      await fetch("/api/save", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      await fetch("/api/done", { method: "POST" });
    } catch (err) {
      console.error("Done error:", err);
    }
  }, [excalidrawAPI]);

  // Keyboard shortcut: Cmd/Ctrl+S to save
  useEffect(() => {
    const handler = (e) => {
      if ((e.metaKey || e.ctrlKey) && e.key === "s") {
        e.preventDefault();
        saveToServer();
      }
    };
    window.addEventListener("keydown", handler);
    return () => window.removeEventListener("keydown", handler);
  }, [saveToServer]);

  if (!initialData) {
    return <div style={{ padding: 20 }}>Loading...</div>;
  }

  return (
    <div style={{ height: "100vh", display: "flex", flexDirection: "column" }}>
      <div
        style={{
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          padding: "4px 12px",
          background: "#f5f5f5",
          borderBottom: "1px solid #ddd",
          fontSize: 13,
        }}
      >
        <span>
          <strong>{CONFIG.file}</strong>
        </span>
        <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
          {lastSaved && (
            <span style={{ color: "#666" }}>Saved {lastSaved}</span>
          )}
          {saving && <span style={{ color: "#999" }}>Saving...</span>}
          <button
            onClick={saveToServer}
            disabled={saving}
            style={{
              padding: "4px 12px",
              cursor: "pointer",
              borderRadius: 4,
              border: "1px solid #ccc",
              background: "#fff",
            }}
          >
            Save
          </button>
          <button
            onClick={handleDone}
            style={{
              padding: "4px 12px",
              cursor: "pointer",
              borderRadius: 4,
              border: "1px solid #4a9",
              background: "#4a9",
              color: "#fff",
              fontWeight: "bold",
            }}
          >
            Done ✓
          </button>
        </div>
      </div>
      <div style={{ flex: 1 }}>
        <Excalidraw
          initialData={initialData}
          excalidrawAPI={(api) => setExcalidrawAPI(api)}
          UIOptions={{
            canvasActions: {
              loadScene: false,
              export: false,
              saveToActiveFile: false,
            },
          }}
        />
      </div>
    </div>
  );
}

ReactDOM.createRoot(document.getElementById("root")).render(<App />);
