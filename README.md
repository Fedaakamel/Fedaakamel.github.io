# Fida Ozkok Portfolio Website

A modern, professional portfolio website built with HTML, CSS, and JavaScript.

## 📁 Files Structure

Your portfolio consists of 3 main files:

```
Fedaakamel.github.io/
├── index.html        (Main HTML file)
├── styles.css        (All styling)
└── script.js         (Interactive features)
```

## 🚀 Step-by-Step Installation Guide

### Step 1: Prepare Your Files

1. **Download** the three files:
   - `index.html`
   - `styles.css`
   - `script.js`

2. **Make sure all three files are in the same folder** - they must be at the root level of your repository.

### Step 2: Upload to GitHub

#### Option A: Using GitHub Web Interface (Easiest)

1. **Go to your repository**: https://github.com/Fedaakamel/Fedaakamel.github.io

2. **Delete old files** (if any exist):
   - Click on each old file
   - Click the trash icon
   - Commit the deletion

3. **Upload new files**:
   - Click "Add file" → "Upload files"
   - Drag and drop all 3 files (`index.html`, `styles.css`, `script.js`)
   - Add a commit message like "Update portfolio design"
   - Click "Commit changes"

#### Option B: Using Git Command Line

```bash
# Navigate to your local repository
cd Fedaakamel.github.io

# Remove old files (if any)
git rm -r *

# Add new files
# (Place index.html, styles.css, and script.js in this folder first)
git add index.html styles.css script.js

# Commit changes
git commit -m "Update portfolio with new design"

# Push to GitHub
git push origin main
```

### Step 3: Verify GitHub Pages Settings

1. Go to your repository on GitHub
2. Click "Settings" tab
3. Click "Pages" in the left sidebar
4. Under "Source", make sure:
   - Branch is set to `main` (or `master`)
   - Folder is set to `/ (root)`
5. Click "Save" if you made changes

### Step 4: Wait for Deployment

- GitHub Pages takes **2-5 minutes** to build and deploy
- You'll see a message "Your site is ready to be published at..."
- Once it says "Your site is live at...", your website is ready!

### Step 5: Visit Your Website

Go to: **https://fedaakamel.github.io/**

---

## 🎨 Customization Guide

### Update Your Personal Information

#### 1. Change Your Name and Title

In `index.html`, find line 33-37:
```html
<h1 class="hero-title">
    <span class="title-line">FIDA</span>
    <span class="title-line">OZKOK</span>
</h1>
<p class="hero-subtitle">Data Scientist & Analytics Expert</p>
```

#### 2. Update Your Description

In `index.html`, find line 38-43:
```html
<p class="hero-description">
    A strong believer in community-based initiatives,<br>
    With a lifetime love of relational mathematics and pattern recognition,<br>
    I succeed in digging into complex data sets and producing<br>
    insightful, data-driven strategic recommendations.
</p>
```

#### 3. Update Contact Information

In `index.html`, find line 180-200 and update:
```html
<a href="mailto:fida@example.com" class="contact-item">
    <!-- Change to your email -->
</a>
```

#### 4. Add Your Social Links

Update GitHub and LinkedIn links around line 191-204.

#### 5. Update Projects

Each project is in a `<article class="project-card">` section. Update:
- Project images (currently using placeholder images from Unsplash)
- Project titles
- Project descriptions
- Project tags

### Change Colors

In `styles.css`, find the `:root` section (line 7-16):

```css
:root {
    --color-primary: #1a1a1a;      /* Main dark color */
    --color-accent: #d4a574;        /* Gold/bronze accent */
    --color-background: #ffffff;    /* Background color */
}
```

Change these hex codes to your preferred colors.

### Change Fonts

In `index.html`, line 8-9, you can replace the Google Fonts:

Current fonts:
- **Playfair Display** (elegant serif for titles)
- **DM Sans** (clean sans-serif for body text)

To change fonts:
1. Visit [Google Fonts](https://fonts.google.com/)
2. Select your fonts
3. Copy the `<link>` code
4. Replace lines 8-9 in `index.html`
5. Update `styles.css` line 14-15 with new font names

---

## 📸 Replace Images

### Current Images (Placeholders)

The website uses Unsplash placeholder images. Replace them with your own:

#### About Section Image (line 100):
```html
<img src="https://images.unsplash.com/photo-1573496359142-b8d87734a5a2?w=600&h=600&fit=crop" alt="Fida Ozkok">
```

**How to add your image:**

1. **Upload your image** to your repository:
   - Create a folder called `images`
   - Upload your photo there
   - Name it something like `profile.jpg`

2. **Update the HTML**:
   ```html
   <img src="images/profile.jpg" alt="Fida Ozkok">
   ```

#### Project Images (lines 148, 164, 180):

Same process - upload project images and update the `src` attribute.

---

## 🔧 Common Issues & Solutions

### Issue 1: Website Not Showing
**Solution**: 
- Wait 5 minutes after uploading
- Check GitHub Pages is enabled in Settings
- Make sure files are in the root folder (not in a subfolder)

### Issue 2: Styling Not Working
**Solution**: 
- Check that `styles.css` is in the same folder as `index.html`
- File names are case-sensitive: must be exactly `styles.css`
- Clear your browser cache (Ctrl+Shift+R or Cmd+Shift+R)

### Issue 3: JavaScript Not Working
**Solution**: 
- Check that `script.js` is in the same folder
- Open browser console (F12) to check for errors
- Make sure filename is exactly `script.js`

### Issue 4: Mobile Menu Not Working
**Solution**: 
- This is controlled by JavaScript
- Make sure `script.js` is properly linked
- Try on a different device or browser

---

## 📱 Features Included

### ✅ Responsive Design
- Works on desktop, tablet, and mobile
- Mobile-friendly navigation menu
- Optimized images and layouts

### ✅ Smooth Animations
- Fade-in effects on scroll
- Smooth scrolling between sections
- Hover effects on buttons and images
- Floating gradient orbs in hero section

### ✅ Interactive Elements
- Sticky navigation that hides on scroll down
- Contact form validation
- Project cards with hover effects
- Active link highlighting

### ✅ Professional Design
- Modern typography
- Clean, minimal layout
- Professional color scheme
- High-quality visual hierarchy

---

## 🎯 Next Steps

### 1. Replace Placeholder Content
- [ ] Update all text with your information
- [ ] Add your real projects
- [ ] Upload your actual photos
- [ ] Add your resume/CV link

### 2. Add More Sections (Optional)
- [ ] Skills/Technologies section
- [ ] Testimonials
- [ ] Blog posts
- [ ] Case studies

### 3. Connect Social Media
- [ ] Link your GitHub profile
- [ ] Link your LinkedIn
- [ ] Add Instagram if relevant
- [ ] Add contact email

### 4. SEO Optimization
- [ ] Add meta descriptions
- [ ] Add favicon
- [ ] Add social media preview images
- [ ] Submit to Google Search Console

---

## 💡 Tips for Success

1. **Keep it updated**: Regularly add new projects and achievements
2. **Quality over quantity**: Showcase your best 3-5 projects
3. **Professional photos**: Use high-quality images
4. **Mobile testing**: Always test on mobile devices
5. **Load time**: Optimize images (compress them before uploading)
6. **Proofread**: Check for spelling and grammar errors

---

## 📞 Need Help?

If you encounter any issues:

1. Check this README carefully
2. Verify all files are uploaded correctly
3. Wait 5 minutes for GitHub Pages to rebuild
4. Clear your browser cache
5. Try a different browser
6. Check the browser console for errors (press F12)

---

## 📄 License

This template is free to use for personal portfolios.

---

**Good luck with your portfolio! 🚀**
