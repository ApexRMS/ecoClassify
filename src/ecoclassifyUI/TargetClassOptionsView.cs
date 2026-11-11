// Target Class Options (ecoClassify_TargetClassOptions): pick one class out of a multi-class
// user classified raster and say which other values are background or ignored.

using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace ecoclassifyUI
{
    public class TargetClassOptionsView : EcoClassifyViewBase
    {
        private const string COL_USE = "useTargetClass";
        private const string COL_VALUE = "targetClassValue";
        private const string COL_LABEL = "targetClassLabel";
        private const string COL_BACKGROUND = "backgroundValues";
        private const string COL_IGNORE = "ignoreValues";

        private readonly CheckBox m_Use;
        private readonly Label m_BinaryNote;
        private readonly TextBox m_Value;
        private readonly Label m_ValueLabel;
        private readonly TextBox m_Label;
        private readonly Label m_LabelLabel;
        private readonly TextBox m_Background;
        private readonly Label m_BackgroundLabel;
        private readonly Label m_BackgroundStatus;
        private readonly TextBox m_Ignore;
        private readonly Label m_IgnoreLabel;
        private readonly Label m_IgnoreStatus;

        public TargetClassOptionsView()
        {
            // -- Mode
            var g = this.AddGroup("User classified rasters");

            this.m_Use = Toggle("Classify one target class from a multi-class raster");
            this.AddFullRow(g, this.m_Use,
                "Off: the user classified rasters are binary. Their lowest value is treated as absence (0) and " +
                "their highest value as presence (1).\r\n" +
                "On: the rasters hold several classes (e.g. 1 = pine, 2 = spruce, ...). The target class becomes " +
                "presence (1) and the other classes become absence (0), unless listed below.");

            this.m_BinaryNote = Note(
                "Binary mode: the lowest value in each user classified raster is absence and the highest is presence.");
            this.AddFullRow(g, this.m_BinaryNote);

            // -- Target
            g = this.AddGroup("Target class");

            this.m_Value = NumberBox(false, 80);
            this.m_ValueLabel = this.AddRow(g, "Target class value", this.m_Value,
                "The raster value of the class to model. These pixels become presence (1). The run stops with a " +
                "warning if this value is not found in any user classified raster.");

            this.m_Label = TextField(200);
            this.m_LabelLabel = this.AddRow(g, "Target class label", this.m_Label,
                "Optional name for the class (e.g. \"Pine\"). Shown in the prediction summary and model " +
                "evaluation outputs.");

            // -- Other values
            g = this.AddGroup("Other class values",
                "Enter whole numbers separated by commas, e.g. 2, 3, 5. Leave both blank to treat every other " +
                "class as absence.");

            this.m_Background = TextField(200);
            this.m_BackgroundStatus = StatusLabel();
            this.m_BackgroundLabel = this.AddRow(g, "Background (absence) values",
                WithStatus(this.m_Background, this.m_BackgroundStatus),
                "Only these values become absence (0). All other non-target values are left out of training " +
                "(treated as no data). When set, the ignore list is not used.");

            this.m_Ignore = TextField(200);
            this.m_IgnoreStatus = StatusLabel();
            this.m_IgnoreLabel = this.AddRow(g, "Ignore values", WithStatus(this.m_Ignore, this.m_IgnoreStatus),
                "Values left out of training (treated as no data), e.g. unlabelled or mixed pixels. Every other " +
                "non-target value becomes absence (0). Only used when the background list is blank.");

            this.m_Background.TextChanged += (s, e) => this.UpdateDependentStates();
            this.m_Ignore.TextChanged += (s, e) => this.UpdateListStatus();

            this.AddValidator(COL_BACKGROUND, ListValidator("Background values"));
            this.AddValidator(COL_IGNORE, ListValidator("Ignore values"));
        }

        protected override void BindControls()
        {
            this.SetCheckBoxBinding(this.m_Use, COL_USE);
            this.SetTextBoxBinding(this.m_Value, COL_VALUE);
            this.SetTextBoxBinding(this.m_Label, COL_LABEL);
            this.SetTextBoxBinding(this.m_Background, COL_BACKGROUND);
            this.SetTextBoxBinding(this.m_Ignore, COL_IGNORE);
        }

        protected override void UpdateDependentStates()
        {
            bool on = this.m_Use.Checked;
            bool hasBackground = !string.IsNullOrWhiteSpace(this.m_Background.Text);

            this.m_BinaryNote.Visible = !on;
            this.SetDependent(on, this.m_Value, this.m_ValueLabel, this.m_Label, this.m_LabelLabel,
                this.m_Background, this.m_BackgroundLabel);

            // The R code ignores the ignore list whenever a background list is given.
            this.SetDependent(on && !hasBackground, this.m_Ignore, this.m_IgnoreLabel);
            this.UpdateListStatus();
        }

        private void UpdateListStatus()
        {
            bool on = this.m_Use.Checked;
            SetListStatus(this.m_BackgroundStatus, this.m_Background.Text, on);
            SetListStatus(this.m_IgnoreStatus,
                string.IsNullOrWhiteSpace(this.m_Background.Text) ? this.m_Ignore.Text : string.Empty, on);

            if (on && !string.IsNullOrWhiteSpace(this.m_Background.Text) && !string.IsNullOrWhiteSpace(this.m_Ignore.Text))
            {
                this.m_IgnoreStatus.Text = "Not used (background values are set)";
                this.m_IgnoreStatus.ForeColor = Color.DarkOrange;
            }
        }

        private static void SetListStatus(Label status, string text, bool active)
        {
            if (!active || string.IsNullOrWhiteSpace(text))
            {
                status.Text = string.Empty;
                return;
            }

            var values = ParseIntList(text);

            if (values == null)
            {
                status.Text = "Use whole numbers separated by commas";
                status.ForeColor = Color.Firebrick;
            }
            else
            {
                status.Text = values.Count == 1 ? "1 value" : values.Distinct().Count() + " values";
                status.ForeColor = Color.ForestGreen;
            }
        }

        private static System.Func<string, string> ListValidator(string what)
        {
            return v => ParseIntList(v) == null
                ? what + " must be whole numbers separated by commas, e.g. 2, 3, 5."
                : null;
        }

        private static Label StatusLabel()
        {
            return new Label { AutoSize = true, Margin = new Padding(6, 6, 3, 3) };
        }

        private static Control WithStatus(Control box, Label status)
        {
            var panel = new FlowLayoutPanel { AutoSize = true, WrapContents = false, Margin = Padding.Empty, Anchor = AnchorStyles.Left };
            panel.Controls.Add(box);
            panel.Controls.Add(status);
            return panel;
        }
    }
}
