# CLL FCL 與 FFL 標高標記

![CLL](assets/banner.svg)

[工具儲存庫](https://github.com/whos-dwg/CadLISP-CLL) · [全部工具索引](https://github.com/whos-dwg/CadLISP-Index)

從框選 DIMENSION 的有效延伸線端點放置 A1-CL-LEVEL 標記，支援 FCL 與 FFL 模式。

| 項目 | 內容 |
| --- | --- |
| 指令 | `CLL` |
| 檔案 | [CLL.lsp](lsp/CLL.lsp) |
| 分類 | 尺寸與註解 |
| 狀態 | 來源快照，實機回歸待完成 |
| 授權 | [MIT](LICENSE)，Copyright (c) 2026 whos-dwg |

[安裝](#安裝) · [操作](#操作) · [相依與設定](#相依與設定) · [已知問題](docs/KNOWN_ISSUES.md) · [驗證紀錄](docs/VALIDATION.md)

## 安裝

以 Windows 桌面版 AutoCAD 為目標。原碼使用的 Visual LISP／ActiveX、DCL 或原生命令，需在目標 AutoCAD 版本分別確認；尚未做整包年份、LT、Mac 或其他 CAD 的相容性認證。ActiveX 的平台限制見 [Autodesk](https://help.autodesk.com/cloudhelp/2026/ENU/AutoCAD-AutoLISP/files/GUID-A0459510-CE7A-4206-9EAA-E25AAB569B20.htm)。

1. 下載並解壓縮本工具到固定位置，先開啟測試圖面。
2. 依公司環境把 `lsp` 資料夾加入需要的支援搜尋路徑與受信任位置。
3. 輸入 `APPLOAD`，載入 `CLL.lsp`；關閉視窗後輸入 `CLL`。

也可使用完整路徑單獨載入；下列路徑需依實際位置修改：

```lisp
(load "C:/CAD/CadLISP-CLL/lsp/CLL.lsp")
```

完成驗證後，可將這支 LISP 加入 APPLOAD 的 Startup Suite。請維持既定的 SECURELOAD 政策，使用受信任位置處理載入限制。[SECURELOAD 說明](https://help.autodesk.com/cloudhelp/2026/ENU/AutoCAD-Core/files/GUID-541566C6-2738-49DD-87C3-C1490E924A02.htm)。

## 操作

1. 輸入 `CLL`，輸入 `S`，選 FCL、FFL 或 Zero；首次使用必須指定標高零點。
2. 重新進入框選流程，指定兩個角點，選入尺寸物件。
3. 檢查數值、最低點的 FFL 標記及翻轉方向。

## 相依與設定

圖檔必須已有 `A1-CL-LEVEL` 圖塊；常用屬性包含 `LEVEL`／`LEVLE`、`TYP`／`TYPE`，動態翻轉參數需配合原圖塊。

圖面影響：新增標高圖塊；FFL 可更新最低點既有標記。

## 行為與限制

FCL 數值以目前 UCS 的 Y 座標相對零點計算，取最接近的 5。FFL 流程將最低點設為 F.F.L. 並清空該標記的 LEVEL，其餘點沿用 FCL 邏輯。檔頭 v23 與載入訊息 v24 不一致。

## 操作示範

![FCL 與 FFL 標高標記操作示範](assets/demos/cll.gif)

這是既有操作錄影，未與本快照逐段比對；本次不是以錄影作為實機回歸證據。媒體範圍見 [媒體說明](assets/NOTICE.md)。

## 來源與授權

本工具依維護者確認為自行從零撰寫，或以 AI 協作從零開發。程式與本次文件採 [MIT 授權](LICENSE)，允許使用、修改與再散布，須保留授權及著作權聲明。詳細來源紀錄見 [provenance.json](metadata/provenance.json)。

## 回報與維護

回報時附 AutoCAD 完整版本、命令列錯誤與去識別的重現範例。修改前請閱讀 [CONTRIBUTING](CONTRIBUTING.md)；發行步驟見 [PUBLISHING](docs/PUBLISHING.md)。本工具可獨立維護、下載及發布，不依賴其他 CadLISP 工具儲存庫。
