// Filtering (ecoClassify_PostProcessingFilter): neighbour-count clean-up of the binary prediction.

using System.Windows.Forms;

namespace ecoclassifyUI
{
    public class PostProcessingFilterView : EcoClassifyViewBase
    {
        private const string COL_APPLY = "applyFiltering";
        private const string COL_FILTER = "filterValue";
        private const string COL_FILL = "fillValue";

        private readonly CheckBox m_Apply;
        private readonly TextBox m_Filter;
        private readonly Label m_FilterLabel;
        private readonly TextBox m_Fill;
        private readonly Label m_FillLabel;
        private readonly Label m_Note;

        public PostProcessingFilterView()
        {
            var g = this.AddGroup("Filtering",
                "Cleans up the binary (presence/absence) prediction by looking at each pixel's 8 surrounding " +
                "pixels. Produces the 'Filtered' map and raster outputs alongside the unfiltered ones.");

            this.m_Apply = Toggle("Filter the binary prediction");
            this.AddFullRow(g, this.m_Apply,
                "On: write filtered outputs in addition to the unfiltered prediction.\r\n" +
                "Off: only the unfiltered prediction is written.");

            this.m_Filter = NumberBox(false, 60);
            this.m_FilterLabel = this.AddRow(g, "Remove presence with fewer than (neighbours)", this.m_Filter,
                "A presence pixel is changed to absence unless at least this many of its 8 neighbours are also " +
                "presence. Removes isolated, speckled presence pixels. Higher values remove more. " +
                "Defaults to 8 if blank.");

            this.m_Fill = NumberBox(false, 60);
            this.m_FillLabel = this.AddRow(g, "Fill absence with at least (neighbours)", this.m_Fill,
                "An absence pixel is changed to presence if at least this many of its 8 neighbours are presence. " +
                "Fills small holes inside presence patches. Lower values fill more. Defaults to 8 if blank.");

            this.m_Note = Note("Both values are counts of the 8 surrounding pixels, so they range from 0 to 8. " +
                "Removal runs first, then filling.");
            this.AddFullRow(g, this.m_Note);

            this.AddValidator(COL_FILTER, IntRange(0, 8, "The minimum neighbours to keep"));
            this.AddValidator(COL_FILL, IntRange(0, 8, "The minimum neighbours to fill"));
        }

        protected override void BindControls()
        {
            this.SetCheckBoxBinding(this.m_Apply, COL_APPLY);
            this.SetTextBoxBinding(this.m_Filter, COL_FILTER);
            this.SetTextBoxBinding(this.m_Fill, COL_FILL);
        }

        protected override void UpdateDependentStates()
        {
            bool on = this.m_Apply.Checked;
            this.SetDependent(on, this.m_Filter, this.m_FilterLabel, this.m_Fill, this.m_FillLabel);
            this.m_Note.Visible = on;
        }
    }
}
