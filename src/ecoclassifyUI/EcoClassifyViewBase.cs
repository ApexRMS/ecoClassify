// ecoClassify custom SyncroSim Studio views.
// Shared layout, tooltip, dependent-enable, validation and single-row data helpers for the
// package's single-row datasheets. Layout pattern follows wildfireRebuildrUI.WildfireViewBase.

using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Windows.Forms;
using SyncroSim.Core;
using SyncroSim.Core.Forms;

namespace ecoclassifyUI
{
    /// <summary>
    /// Base view: a scrolling stack of group boxes, each a two-column grid of label + control.
    /// Every label and control can carry a tooltip. Derived views build their controls in the
    /// constructor, bind them in <see cref="BindControls"/>, and apply dependent enable rules in
    /// <see cref="UpdateDependentStates"/>, which the base class calls whenever data or the
    /// view's enabled state changes.
    /// </summary>
    public class EcoClassifyViewBase : DataFeedView
    {
        protected readonly ToolTip Tips;
        private readonly FlowLayoutPanel m_Stack;
        private readonly List<Control> m_UnboundControls = new List<Control>();
        private readonly Dictionary<string, Func<string, string>> m_Validators = new Dictionary<string, Func<string, string>>();

        public EcoClassifyViewBase()
        {
            this.Tips = new ToolTip { AutoPopDelay = 20000, InitialDelay = 400, ReshowDelay = 200, ShowAlways = true };

            this.m_Stack = new FlowLayoutPanel
            {
                Dock = DockStyle.Fill,
                FlowDirection = FlowDirection.TopDown,
                WrapContents = false,
                AutoScroll = true,
                Padding = new Padding(6),
            };

            this.Controls.Add(this.m_Stack);
            this.m_Stack.Resize += (s, e) => this.ResizeGroups();
        }

        // ---- layout -----------------------------------------------------------------

        /// <summary>Adds a titled group and returns its two-column grid.</summary>
        protected TableLayoutPanel AddGroup(string title, string tooltip = null)
        {
            var gb = new GroupBox
            {
                Text = title,
                AutoSize = true,
                AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink,
                Padding = new Padding(8, 6, 8, 8),
                Margin = new Padding(3, 3, 3, 9),
                MinimumSize = new Size(520, 0),
            };

            var grid = new TableLayoutPanel
            {
                Dock = DockStyle.Fill,
                AutoSize = true,
                AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink,
                ColumnCount = 2,
            };

            grid.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, 230));
            grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100));
            gb.Controls.Add(grid);
            this.m_Stack.Controls.Add(gb);

            if (tooltip != null)
            {
                this.Tips.SetToolTip(gb, tooltip);
            }

            this.ResizeGroups();
            return grid;
        }

        /// <summary>Adds a label + control row. The tooltip goes on both.</summary>
        protected Label AddRow(TableLayoutPanel grid, string label, Control control, string tooltip)
        {
            var lbl = new Label
            {
                Text = label,
                AutoSize = true,
                Anchor = AnchorStyles.Left,
                Margin = new Padding(3, 7, 3, 3),
            };

            control.Margin = new Padding(3, 3, 3, 3);

            // Stretch only controls that did not ask for a fixed width (number boxes and combos set Left).
            if (control.Anchor == (AnchorStyles.Top | AnchorStyles.Left) && control.Dock == DockStyle.None)
            {
                control.Anchor = AnchorStyles.Left | AnchorStyles.Right;
            }

            int row = grid.RowCount++;
            grid.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            grid.Controls.Add(lbl, 0, row);
            grid.Controls.Add(control, 1, row);

            if (!string.IsNullOrEmpty(tooltip))
            {
                this.Tips.SetToolTip(lbl, tooltip);
                this.SetTipRecursive(control, tooltip);
            }

            return lbl;
        }

        /// <summary>Adds a control that spans both columns (checkboxes, notes, warnings).</summary>
        protected void AddFullRow(TableLayoutPanel grid, Control control, string tooltip = null)
        {
            int row = grid.RowCount++;
            grid.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            grid.Controls.Add(control, 0, row);
            grid.SetColumnSpan(control, 2);

            if (!string.IsNullOrEmpty(tooltip))
            {
                this.SetTipRecursive(control, tooltip);
            }
        }

        protected static Label Note(string text, Color? color = null)
        {
            return new Label
            {
                Text = text,
                AutoSize = true,
                MaximumSize = new Size(600, 0),
                ForeColor = color ?? SystemColors.GrayText,
                Margin = new Padding(3, 2, 3, 6),
            };
        }

        protected static CheckBox Toggle(string text)
        {
            return new CheckBox { Text = text, AutoSize = true, Margin = new Padding(3, 3, 3, 3) };
        }

        protected static TextBox NumberBox(bool allowDecimal, int width = 90)
        {
            var tb = new TextBox { Width = width, Anchor = AnchorStyles.Left };
            tb.KeyPress += allowDecimal ? (KeyPressEventHandler)OnDecimalKeyPress : OnIntegerKeyPress;
            return tb;
        }

        protected static TextBox TextField(int width = 220)
        {
            return new TextBox { Width = width, Anchor = AnchorStyles.Left };
        }

        protected static ComboBox ListBox(int width = 200)
        {
            return new ComboBox { DropDownStyle = ComboBoxStyle.DropDownList, Width = width, Anchor = AnchorStyles.Left };
        }

        /// <summary>
        /// Enables or disables dependent controls. A control is only enabled when the view itself
        /// is editable, so read-only (result) scenarios stay read-only.
        /// </summary>
        protected void SetDependent(bool on, params Control[] controls)
        {
            bool enable = on && this.IsEditable;

            foreach (Control c in controls)
            {
                if (c != null)
                {
                    c.Enabled = enable;
                }
            }
        }

        /// <summary>True when the bound datasheet can be edited (false for result scenarios).</summary>
        protected bool IsEditable
        {
            get { return this.DataFeed != null && this.ShouldEnableView(); }
        }

        /// <summary>
        /// Registers controls that are not bound to a column (buttons) so they follow the view's
        /// enabled state, e.g. read-only for result scenarios.
        /// </summary>
        protected void RegisterUnbound(params Control[] controls)
        {
            this.m_UnboundControls.AddRange(controls);
        }

        // ---- lifecycle ----------------------------------------------------------------

        public override void LoadDataFeed(DataFeed dataFeed)
        {
            base.LoadDataFeed(dataFeed);

            this.BindControls();
            this.AddStandardCommands();

            // Read-only (e.g. result) scenarios never get EnableView(true), so unbound buttons
            // would otherwise stay clickable.
            bool enable = this.ShouldEnableView();

            foreach (Control c in this.m_UnboundControls)
            {
                c.Enabled = enable;
            }

            this.UpdateDependentStates();
        }

        /// <summary>Bind controls to columns here (called once from LoadDataFeed).</summary>
        protected virtual void BindControls()
        {
        }

        /// <summary>Apply dependent enable/visibility rules and status labels here.</summary>
        protected virtual void UpdateDependentStates()
        {
        }

        public override void EnableView(bool enable)
        {
            base.EnableView(enable);

            foreach (Control c in this.m_UnboundControls)
            {
                c.Enabled = enable;
            }

            if (enable)
            {
                this.UpdateDependentStates();
            }
        }

        public override void RefreshControls()
        {
            base.RefreshControls();
            this.UpdateDependentStates();
        }

        protected override void OnBoundCheckBoxChanged(CheckBox checkBox, string columnName)
        {
            base.OnBoundCheckBoxChanged(checkBox, columnName);
            this.UpdateDependentStates();
        }

        protected override void OnBoundComboBoxSelectedValueChanged(ComboBox comboBox, string columnName)
        {
            base.OnBoundComboBoxSelectedValueChanged(comboBox, columnName);
            this.UpdateDependentStates();
        }

        protected override void OnBoundTextBoxValidated(TextBox textBox, string columnName, string newValue)
        {
            base.OnBoundTextBoxValidated(textBox, columnName, newValue);
            this.UpdateDependentStates();
        }

        // ---- validation -----------------------------------------------------------------

        /// <summary>
        /// Adds a check for a bound text box column. The function returns an error message, or
        /// null if the value is fine. Empty values skip the check (nullability is handled by SyncroSim).
        /// </summary>
        protected void AddValidator(string columnName, Func<string, string> validator)
        {
            this.m_Validators[columnName] = validator;
        }

        protected override bool OnBoundTextBoxValidating(TextBox textBox, string columnName, string proposedValue)
        {
            if (!string.IsNullOrWhiteSpace(proposedValue) && this.m_Validators.TryGetValue(columnName, out var check))
            {
                string error = check(proposedValue.Trim());

                if (error != null)
                {
                    MessageBox.Show(this, error, "ecoClassify", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    textBox.SelectionStart = 0;
                    textBox.SelectionLength = textBox.Text.Length;
                    return false;
                }
            }

            return base.OnBoundTextBoxValidating(textBox, columnName, proposedValue);
        }

        protected static Func<string, string> IntRange(int min, int max, string what)
        {
            return v =>
            {
                if (!int.TryParse(v, NumberStyles.Integer, CultureInfo.CurrentCulture, out int i) || i < min || i > max)
                {
                    return string.Format(CultureInfo.CurrentCulture, "{0} must be a whole number from {1} to {2}.", what, min, max);
                }

                return null;
            };
        }

        protected static Func<string, string> DoubleRange(double min, double max, string what)
        {
            return v =>
            {
                if (!double.TryParse(v, NumberStyles.Float, CultureInfo.CurrentCulture, out double d) || d < min || d > max)
                {
                    return string.Format(CultureInfo.CurrentCulture, "{0} must be a number from {1} to {2}.", what, min, max);
                }

                return null;
            };
        }

        /// <summary>
        /// Parses a comma-separated list of whole numbers. Returns null if the text is not a valid list.
        /// </summary>
        protected static List<int> ParseIntList(string text)
        {
            var values = new List<int>();

            foreach (string part in text.Split(','))
            {
                string p = part.Trim();

                if (p.Length == 0 || !int.TryParse(p, NumberStyles.Integer, CultureInfo.InvariantCulture, out int i))
                {
                    return null;
                }

                values.Add(i);
            }

            return values;
        }

        private void ResizeGroups()
        {
            int w = Math.Max(520, this.m_Stack.ClientSize.Width - this.m_Stack.Padding.Horizontal - 12);

            foreach (Control c in this.m_Stack.Controls)
            {
                c.Width = w;
                c.MinimumSize = new Size(w, 0);
                c.MaximumSize = new Size(w, 0);
            }
        }

        private void SetTipRecursive(Control c, string tip)
        {
            this.Tips.SetToolTip(c, tip);

            foreach (Control child in c.Controls)
            {
                this.SetTipRecursive(child, tip);
            }
        }

        // ---- single-row data ---------------------------------------------------------

        protected DataSheet Sheet
        {
            get { return this.DataFeed.DataSheets[0]; }
        }

        protected object GetValue(string column)
        {
            DataRow dr = this.Sheet.GetDataRow();

            if (dr == null || !dr.Table.Columns.Contains(column) || dr[column] == DBNull.Value)
            {
                return null;
            }

            return dr[column];
        }

        protected string GetString(string column)
        {
            object v = this.GetValue(column);
            return v == null ? null : Convert.ToString(v, CultureInfo.InvariantCulture);
        }

        protected void SetValue(string column, object value)
        {
            this.Sheet.SetSingleRowData(column, value);
            this.RefreshControls();
        }

        // ---- shared widgets --------------------------------------------------------------

        /// <summary>
        /// Read-only file name box with Browse... and Clear buttons, bound to an external-file
        /// String column. Browsing copies the file into the library's input folder the same way
        /// SyncroSim's own grids do (file name in the column + AddExternalInputFile).
        /// </summary>
        protected Control FilePicker(string column, string dialogTitle, string filter, out TextBox nameBox)
        {
            // Buttons dock right; the name box fills what is left (it must be front-most to dock last).
            var panel = new Panel { Height = 25, Anchor = AnchorStyles.Left | AnchorStyles.Right, Margin = Padding.Empty };
            var tb = new TextBox { ReadOnly = true, Dock = DockStyle.Fill, BackColor = SystemColors.Window };
            var browse = new Button { Text = "Browse...", Width = 80, Dock = DockStyle.Right };
            var clear = new Button { Text = "Clear", Width = 60, Dock = DockStyle.Right };

            panel.Controls.Add(browse);
            panel.Controls.Add(clear);
            panel.Controls.Add(tb);
            tb.BringToFront();

            browse.Click += (s, e) =>
            {
                using (var dlg = new OpenFileDialog { Title = dialogTitle, Filter = filter })
                {
                    if (dlg.ShowDialog(this) != DialogResult.OK)
                    {
                        return;
                    }

                    this.Sheet.SetSingleRowData(column, Path.GetFileName(dlg.FileName));
                    this.Sheet.AddExternalInputFile(dlg.FileName);
                    this.RefreshControls();
                }
            };

            clear.Click += (s, e) => this.SetValue(column, null);

            this.RegisterUnbound(browse, clear);
            nameBox = tb;
            return panel;
        }

        // ---- key filters ---------------------------------------------------------------

        protected static void OnIntegerKeyPress(object sender, KeyPressEventArgs e)
        {
            if (!char.IsControl(e.KeyChar) && !char.IsDigit(e.KeyChar) && e.KeyChar != '-')
            {
                e.Handled = true;
            }
        }

        protected static void OnDecimalKeyPress(object sender, KeyPressEventArgs e)
        {
            string sep = CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator;

            if (char.IsControl(e.KeyChar) || char.IsDigit(e.KeyChar) || e.KeyChar == '-')
            {
                return;
            }

            if (e.KeyChar.ToString() == sep && !((TextBox)sender).Text.Contains(sep))
            {
                return;
            }

            e.Handled = true;
        }
    }
}
